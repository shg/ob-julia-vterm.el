;;; ob-julia-vterm.el --- Babel Functions for Julia in VTerm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: October 31, 2020
;; URL: https://github.com/shg/ob-julia-vterm.el
;; Package-Requires: ((emacs "26.1") (julia-vterm "0.10"))
;; Version: 0.2
;; Keywords: julia, org, outlines, literate programming, reproducible research

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Org-Babel support for Julia source code block using julia-vterm.

;;; Requirements:

;; This package uses julia-vterm to run Julia code.
;;
;; - https://github.com/shg/julia-vterm.el
;;
;; See https://github.com/shg/ob-julia-vterm.el for installation
;; instructions.

;;; Code:

(require 'ob)
(require 'queue)
(require 'filenotify)
(require 'julia-vterm)

(defvar org-babel-julia-vterm-debug nil)

(defun org-babel-julia-vterm--wrap-body (session body)
  "Make Julia code that execute-s BODY and obtains the results, depending on SESSION."
  (concat
   (if session "" "let\n")
   body
   (if session "" "\nend\n")))

(defun org-babel-julia-vterm--make-str-to-run (result-type src-file out-file)
  "Make Julia code that load-s SRC-FILE and save-s the result to OUT-FILE, depending on RESULT-TYPE."
  (format
   (pcase result-type
     ('output "\
using Logging: Logging; let
    out_file = \"%s\"
    result = open(out_file, \"w\") do io
        logger = Logging.ConsoleLogger(io)
        redirect_stdout(io) do
            try
                include(\"%s\")
            catch e
                Logging.with_logger(logger) do
                    @error e.error
                end
            end
        end
    end
    open(io -> println(read(io, String)), out_file)
    result
end\n")
     ('value "\
using Logging: Logging
open(\"%s\", \"w\") do io
    logger = Logging.ConsoleLogger(io)
    try
        result = include(\"%s\")
        print(io, result)
        result
    catch e
        Logging.with_logger(logger) do
            @error e.error
        end
        e.error
    end
end\n"))
   out-file src-file))

(defun org-babel-execute:julia-vterm (body params)
  "Execute a block of Julia code with Babel.
This function is called by `org-babel-execute-src-block'.
BODY is the contents and PARAMS are header arguments of the code block."
  (let* ((session-name (cdr (assq :session params)))
	 (session (pcase session-name ('nil "main") ("none" nil) (_ session-name)))
	 (var-lines (org-babel-variable-assignments:julia-vterm params)))
    (org-babel-julia-vterm-evaluate (current-buffer)
				    session
				    (org-babel-expand-body:generic body params var-lines)
				    params)))

(defun org-babel-variable-assignments:julia-vterm (params)
  "Return list of Julia statements assigning variables based on variable-value pairs in PARAMS."
  (mapcar
   (lambda (pair) (format "%s = %s" (car pair) (cdr pair)))
   (org-babel--get-vars params)))

(defun org-babel-julia-vterm--check-long-line (str)
  "Return t if STR is too long for stable output in the REPL."
  (catch 'loop
    (dolist (line (split-string str "\n"))
      (if (> (length line) 12000)
	  (throw 'loop t)))))

(defvar-local org-babel-julia-vterm--evaluation-queue nil)
(defvar-local org-babel-julia-vterm--evaluation-watches nil)

(defun org-babel-julia-vterm--add-evaluation-to-evaluation-queue (evaluation)
  "Add an EVALUATION of a source block to queue."
  (if (not (queue-p org-babel-julia-vterm--evaluation-queue))
      (setq org-babel-julia-vterm--evaluation-queue (queue-create)))
  (queue-append org-babel-julia-vterm--evaluation-queue evaluation))

(defun org-babel-julia-vterm--evaluation-completed-callback-func ()
  "Return a callback function that is called when the result is written to the file."
  (lambda (event)
    (let ((current (queue-first org-babel-julia-vterm--evaluation-queue)))
      (let-alist current
	(save-excursion
	  (with-current-buffer .buf
	    (goto-char .src-block-begin)
	    (if (and (not (equal .src-block-begin .src-block-end))
		     (or (eq (org-element-type (org-element-context)) 'src-block)
			 (eq (org-element-type (org-element-context)) 'inline-src-block)))
		(let ((bs (with-temp-buffer
			    (insert-file-contents .out-file)
			    (buffer-string))))
		  (cond ((member "file" (cdr (assq :result-params .params)))
			 (org-redisplay-inline-images))
			(t
			 (if (org-babel-julia-vterm--check-long-line bs)
			     "Output suppressed (line too long)"
			   (org-babel-insert-result bs '("replace")))))))
	    (queue-dequeue org-babel-julia-vterm--evaluation-queue)
	    (setq org-babel-julia-vterm--evaluation-watches
		  (delete (assoc .uuid org-babel-julia-vterm--evaluation-watches)
			  org-babel-julia-vterm--evaluation-watches))
	    (sit-for 0.1)
	    (org-babel-julia-vterm--process-evaluation-queue)))))))

(defun org-babel-julia-vterm--clear-evaluation-queue ()
  "Clear the evaluation queue and watches."
  (if (queue-p org-babel-julia-vterm--evaluation-queue)
      (queue-clear org-babel-julia-vterm--evaluation-queue))
  (setq org-babel-julia-vterm--evaluation-watches '()))

(defun org-babel-julia-vterm--process-evaluation-queue ()
  "Process the evaluation queue."
  (if (and (queue-p org-babel-julia-vterm--evaluation-queue)
	   (not (queue-empty org-babel-julia-vterm--evaluation-queue)))
      (if (eq (julia-vterm-fellow-repl-buffer-status) :julia)
	  (let ((current (queue-first org-babel-julia-vterm--evaluation-queue)))
	    (let-alist current
	      (unless (assoc .uuid org-babel-julia-vterm--evaluation-watches)
		(let ((desc (file-notify-add-watch
			     .out-file '(change)
			     (org-babel-julia-vterm--evaluation-completed-callback-func))))
		  (push (cons .uuid desc) org-babel-julia-vterm--evaluation-watches))
		(julia-vterm-paste-string
		 (org-babel-julia-vterm--make-str-to-run (cdr (assq :result-type .params))
							 .src-file .out-file)
		 .session))))
	(run-at-time 1 nil #'org-babel-julia-vterm--process-evaluation-queue))))

(defun org-babel-julia-vterm-evaluate (buf session body params)
  "Evaluate BODY as Julia code in a julia-vterm buffer specified with SESSION."
  (let ((uuid (org-id-uuid))
	(src-file (org-babel-temp-file "julia-vterm-src-"))
	(out-file (org-babel-temp-file "julia-vterm-out-"))
	(src (org-babel-julia-vterm--wrap-body session body)))
    (with-temp-file src-file (insert src))
    (when org-babel-julia-vterm-debug
      (julia-vterm-paste-string
       (format "#= params ======\n%s\n== src =========\n%s\n===============#\n" params src)
       session))
    (let ((elm (org-element-context))
	  (src-block-begin (make-marker))
	  (src-block-end (make-marker)))
      (set-marker src-block-begin (org-element-property :begin elm))
      (set-marker src-block-end (org-element-property :end elm))
      (org-babel-julia-vterm--add-evaluation-to-evaluation-queue
       (list (cons 'uuid uuid)
	     (cons 'buf buf)
	     (cons 'session session)
	     (cons 'params params)
	     (cons 'src-file src-file)
	     (cons 'out-file out-file)
	     (cons 'src-block-begin src-block-begin)
	     (cons 'src-block-end src-block-end))))
    (sit-for 0.2)
    (org-babel-julia-vterm--process-evaluation-queue)
    (concat "Executing... " (substring uuid 0 8))))

(add-to-list 'org-src-lang-modes '("julia-vterm" . "julia"))

(provide 'ob-julia-vterm)

;;; ob-julia-vterm.el ends here
