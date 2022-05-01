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
	 (result-type (cdr (assq :result-type params)))
	 (var-lines (org-babel-variable-assignments:julia-vterm params))
	 (full-body (org-babel-expand-body:generic body params var-lines))
	 (session (pcase session-name ('nil "main") ("none" nil) (_ session-name))))
    (org-babel-julia-vterm-evaluate session full-body result-type params)))

(defun org-babel-variable-assignments:julia-vterm (params)
  "Return list of Julia statements assigning variables based on variable-value pairs in PARAMS."
  (mapcar
   (lambda (pair) (format "%s = %s" (car pair) (cdr pair)))
   (org-babel--get-vars params)))

(defun org-babel-julia-vterm--wait-for-output (file &optional timeout)
  "Wait until the FILE is written or TIMEOUT seconds have elapsed."
  (let ((c 0)
	(timeout (or timeout 10))
	(interval 0.1))
    (while (and (< c (/ timeout interval)) (= 0 (file-attribute-size (file-attributes file))))
      (sit-for interval)
      (setq c (1+ c)))))

(defun org-babel-julia-vterm--check-long-line (str)
  (catch 'loop
    (dolist (line (split-string str "\n"))
      (if (> (length line) 12000)
	  (throw 'loop t)))))

(defvar-local org-babel-julia-vterm--evaluation-queue nil)
(defvar-local org-babel-julia-vterm--evaluation-watches nil)

(defun org-babel-julia-vterm--add-evaluation-to-evaluation-queue
    (uuid session result-type params src-file out-file buf srcfrom srcto)
  (message "objv--add-evaluation-to-evaluation-queue called")
  (if (not (queue-p org-babel-julia-vterm--evaluation-queue))
      (setq org-babel-julia-vterm--evaluation-queue (queue-create)))
  (queue-append org-babel-julia-vterm--evaluation-queue
		(list uuid session result-type params src-file out-file buf srcfrom srcto)))

(defun org-babel-julia-vterm--evaluation-completed-callback-func ()
  (lambda (event)
    (let ((current (queue-first org-babel-julia-vterm--evaluation-queue)))
      (let ((uuid        (nth 0 current))
	    (params      (nth 3 current))
	    (out-file    (nth 5 current))
	    (buf         (nth 6 current))
	    (srcfrom     (nth 7 current))
	    (srcto       (nth 8 current)))
	(message "callback func called for %s" uuid)
	(save-excursion
	  (with-current-buffer buf
	    ;; (message "2 src from = %s, to = %s" srcfrom srcto)
	    (if (and (not (equal srcfrom srcto))
		     (or (eq (org-element-type (org-element-context)) 'src-block)
			 (eq (org-element-type (org-element-context)) 'inline-src-block)))
		(let ((bs (with-temp-buffer
			    (insert-file-contents out-file)
			    (buffer-string)))
		      (result-params (cdr (assq :result-params params))))
		  (message "result-params = " result-params)
		  (cond ((member "file" result-params)
			 (org-redisplay-inline-images))
			(t
			 (if (org-babel-julia-vterm--check-long-line bs)
			     "Output suppressed (line too long)"
			   (goto-char srcfrom)
			   (org-babel-insert-result bs '("replace")))))
		  (queue-dequeue org-babel-julia-vterm--evaluation-queue)
		  (setq org-babel-julia-vterm--evaluation-watches
			(delete (assoc uuid org-babel-julia-vterm--evaluation-watches)
				org-babel-julia-vterm--evaluation-watches))
		  (sit-for 0.1)
		  (org-babel-julia-vterm--process-evaluation-queue)))))))))

(defun org-babel-julia-vterm--clear-evaluation-queue ()
  (message "objv--clear-evaluation-queue called")
  (if (queue-p org-babel-julia-vterm--evaluation-queue)
      (queue-clear org-babel-julia-vterm--evaluation-queue))
  (setq org-babel-julia-vterm--evaluation-watches '())
  (message "Evaluation queue cleared"))

(defun org-babel-julia-vterm--process-evaluation-queue ()
  (message "objv--process-evaluation-queue called")
  (if (eq (julia-vterm-fellow-repl-buffer-status) :julia)
      (when (and (queue-p org-babel-julia-vterm--evaluation-queue)
		 (not (queue-empty org-babel-julia-vterm--evaluation-queue)))
	(let ((current (queue-first org-babel-julia-vterm--evaluation-queue)))
	  (let ((uuid        (nth 0 current))
		(session     (nth 1 current))
		(result-type (nth 2 current))
		(src-file    (nth 4 current))
		(out-file    (nth 5 current)))
	    (unless (assoc uuid org-babel-julia-vterm--evaluation-watches)
	      (message "Register file notify for %s" uuid)
	      (let ((desc (file-notify-add-watch
			   out-file '(change)
			   (org-babel-julia-vterm--evaluation-completed-callback-func))))
		(push (cons uuid desc) org-babel-julia-vterm--evaluation-watches))
	      ;; (when org-babel-julia-vterm-debug
	      ;;   (julia-vterm-paste-string
	      ;;    (format "#= src-file ======\n%s===============#\n"
	      ;; 	     (org-babel-julia-vterm--make-str-to-run result-type src-file out-file))
	      ;;    session))
	      (message "Run code for %s" uuid)
	      (julia-vterm-paste-string
	       (org-babel-julia-vterm--make-str-to-run result-type src-file out-file)
	       session)))))
    ;; (message "watches = %S" org-babel-julia-vterm--evaluation-watches)
    ;; (message "queue = %S" org-babel-julia-vterm--evaluation-queue)
    (message "REPL is not ready. Wait for 1 seconds.")
    ;; (org-babel-julia-vterm--clear-evaluation-queue)
    (run-at-time 1 nil #'org-babel-julia-vterm--process-evaluation-queue)))

(defun org-babel-julia-vterm-evaluate (session body result-type params)
  "Evaluate BODY as Julia code in a julia-vterm buffer specified with SESSION."
  (let ((src-file (org-babel-temp-file "julia-vterm-src-"))
	(out-file (org-babel-temp-file "julia-vterm-out-"))
	(src (org-babel-julia-vterm--wrap-body session body))
	(elm (org-element-context))
	(uuid (org-id-uuid)))
    (message "------------------------------")
    (message "objv-evaluate called" )
    (message "------------------------------")
    (message "src code from %s to %s" (org-element-property :begin elm) (org-element-property :end elm))
    (with-temp-file src-file (insert src))
    (when org-babel-julia-vterm-debug
      (julia-vterm-paste-string
       (format "#= params ======\n%s\n== src =========\n%s\n===============#\n" params src)
       session))
    (let ((srcfrom (make-marker))
	  (srcto (make-marker)))
      (set-marker srcfrom (org-element-property :begin elm))
      (set-marker srcto (org-element-property :end elm))
      ;; (message "1 src from = %s, to = %s" srcfrom srcto)
      (org-babel-julia-vterm--add-evaluation-to-evaluation-queue
       uuid session result-type params src-file out-file (current-buffer) srcfrom srcto))
    (sit-for 0.2)
    (org-babel-julia-vterm--process-evaluation-queue)
    (concat "Executing... " (substring uuid 0 8))))

(add-to-list 'org-src-lang-modes '("julia-vterm" . "julia"))

(provide 'ob-julia-vterm)

;;; ob-julia-vterm.el ends here
