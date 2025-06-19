;;; ob-julia-vterm.el --- Babel functions for Julia that work with julia-vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: October 31, 2020
;; URL: https://github.com/shg/ob-julia-vterm.el
;; Package-Requires: ((emacs "26.1") (julia-vterm "0.26") (queue "0.2"))
;; Version: 0.11
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
(require 'org-id)
(require 'queue)
(require 'filenotify)
(require 'julia-vterm)

(defun ob-julia-vterm-wrap-body (use-let body)
  "Make Julia code that execute-s BODY and obtains the results, depending on USE-LET."
  (concat
   (if use-let "let\n" "")
   body
   (if use-let "\nend\n" "")))

(defun ob-julia-vterm-make-str-to-run (uuid params src-file out-file stdin)
  "Make Julia code that execute-s the code in SRC-FILE depending on PARAMS.
The results are saved in OUT-FILE.  UUID is a unique id assigned
to the evaluation."
  (format
   (pcase (cdr (assq :result-type params))
     ('output "\
#OB-JULIA-VTERM_BEGIN %s
import Logging; let in_file = %s, out_file = \"%s\"
    open(out_file, \"w\") do io
        logger = Logging.ConsoleLogger(io)
        redirect_stdio(stdin = in_file, stdout = io) do
            try
                include(\"%s\")
                # %s %s
            catch e
                msg = if %s
                    bt = catch_backtrace()
                    i = findfirst(ip -> any(fr.func == Symbol(\"top-level scope\") for fr in StackTraces.lookup(ip)), bt)
                    showerror(logger.stream, e, bt[1:i])
                else
                    showerror(logger.stream, e)
                end
            end
        end
    end
    result = open(io -> println(read(io, String)), out_file)
    if result == nothing
        open(out_file, \"a\") do io
            print(io, \"\n\")
        end
    else
        result
    end
end #OB-JULIA-VTERM_END\n")
     ('value "\
#OB-JULIA-VTERM_BEGIN %s
import Logging; let in_file = %s, out_file = \"%s\"
    open(out_file, \"w\") do io
        logger = Logging.ConsoleLogger(io)
        redirect_stdio(stdin = in_file) do
            try
                result = include(\"%s\")
                if result == \"\"
                    result = \"\n\"
                end
                pp = %s; nolimit = %s
                if pp
                    if nolimit
                        Base.invokelatest(show, io, \"text/plain\", result)
                    else
                        Base.invokelatest(show, IOContext(io, :limit => true), \"text/plain\", result)
                    end
                else
                    if nolimit
                        Base.invokelatest(print, io, result)
                    else
                        Base.invokelatest(print, IOContext(io, :limit => true), result)
                    end
                end
                result
            catch e
                msg = if %s
                    bt = catch_backtrace()
                    i = findfirst(ip -> any(fr.func == Symbol(\"top-level scope\") for fr in StackTraces.lookup(ip)), bt)
                    sprint(showerror, e, bt[1:i])
                else
                    msg = sprint(showerror, e)
                end
                println(logger.stream, msg)
                println(msg)
            end
        end
    end
end #OB-JULIA-VTERM_END\n"))
   (substring uuid 0 8)
   (if stdin (format "\"%s\"" stdin) "nothing")
   out-file src-file
   (if (member "pp" (cdr (assq :result-params params))) "true" "false")
   (if (member "nolimit" (cdr (assq :result-params params))) "true" "false")
   (if (not (member (cdr (assq :debug params)) '(nil "no"))) "true" "false")))

(defun org-babel-execute:julia-vterm (body params)
  "Execute a block of Julia code with Babel.
This function is called by `org-babel-execute-src-block'.
BODY is the contents and PARAMS are header arguments of the code block."
  (let* ((session (ob-julia-vterm-session))
         (use-let (ob-julia-vterm-session-none-p))
	 (var-lines (org-babel-variable-assignments:julia-vterm params))
	 (result-params (cdr (assq :result-params params))))
    (with-current-buffer (julia-vterm-repl-buffer session)
      (add-hook 'julia-vterm-repl-filter-functions #'ob-julia-vterm-output-filter))
    (ob-julia-vterm-evaluate (current-buffer)
			     session
                             use-let
			     (org-babel-expand-body:generic body params var-lines)
			     params)))

(defun org-babel-variable-assignments:julia-vterm (params)
  "Return list of Julia statements assigning variables based on variable-value pairs in PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s = %s" (car pair) (ob-julia-vterm-value-to-julia (cdr pair))))
   (org-babel--get-vars params)))

(defun ob-julia-vterm-escape-string (str)
  "Escape special characters in STR for Julia variable assignments."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun ob-julia-vterm-value-to-julia (value)
  "Convert an emacs-lisp VALUE to a string of julia code for the value."
  (cond
   ((listp value) (format "\"%s\"" value))
   ((numberp value) value)
   ((stringp value) (or (org-babel--string-to-number value)
			(concat "\"" (ob-julia-vterm-escape-string value) "\"")))
   ((symbolp value) (ob-julia-vterm-escape-string (symbol-name value)))
   (t value)))

(defun ob-julia-vterm-check-long-line (value)
  "Return t if VALUE is too long for org-babel result."
  (let ((str (prin1-to-string value)))
    (catch 'loop
      (dolist (line (split-string str "\n"))
	(if (> (length line) 12000)
	    (throw 'loop t))))))

(defvar-local ob-julia-vterm-evaluation-queue nil)
(defvar-local ob-julia-vterm-evaluation-watches nil)

(defun ob-julia-vterm-add-evaluation-to-evaluation-queue (session evaluation)
  "Add an EVALUATION of a source block to SESSION's evaluation queue."
  (with-current-buffer (julia-vterm-repl-buffer session)
    (if (not (queue-p ob-julia-vterm-evaluation-queue))
	(setq ob-julia-vterm-evaluation-queue (queue-create)))
    (queue-append ob-julia-vterm-evaluation-queue evaluation)))

(defun ob-julia-vterm-evaluation-completed-callback-func (session)
  "Return a callback function to be called when an evaluation in SESSION is completed."
  (lambda (event)
    (if (eq 'changed (cadr event))
	(with-current-buffer (julia-vterm-repl-buffer session)
	  (if (and (queue-p ob-julia-vterm-evaluation-queue)
		   (> (queue-length ob-julia-vterm-evaluation-queue) 0))
	      (let-alist (queue-first ob-julia-vterm-evaluation-queue)
		(with-current-buffer .buf
		  (save-excursion
		    (goto-char .src-block-begin)
		    (when (and (not (equal .src-block-begin .src-block-end))
			       (or (eq (org-element-type (org-element-context)) 'src-block)
				   (eq (org-element-type (org-element-context)) 'inline-src-block)))
		      (ob-julia-vterm-wait-for-file-change .out-file 10 0.1)
		      (let ((result (with-temp-buffer
				      (insert-file-contents .out-file)
				      (replace-regexp-in-string "\n\\'" "" (buffer-string))))
			    (result-params (cdr (assq :result-params .params))))
			(cond ((member "file" result-params)
			       (org-redisplay-inline-images))
			      ((not (member "none" result-params))
			       (org-babel-insert-result
				(if (ob-julia-vterm-check-long-line result)
				    "Output suppressed (line too long)"
				  (org-babel-result-cond result-params
				    result
				    (org-babel-reassemble-table
				     result
				     (org-babel-pick-name (cdr (assq :colname-names .params))
							  (cdr (assq :colnames .params)))
				     (org-babel-pick-name (cdr (assq :rowname-names .params))
							  (cdr (assq :rownames .params))))))
				result-params
				(org-babel-get-src-block-info 'light))))))))
		(queue-dequeue ob-julia-vterm-evaluation-queue)
		(file-notify-rm-watch (cdr (assoc .uuid ob-julia-vterm-evaluation-watches)))
		(setq ob-julia-vterm-evaluation-watches
		      (delete (assoc .uuid ob-julia-vterm-evaluation-watches)
			      ob-julia-vterm-evaluation-watches))
		(ob-julia-vterm-process-evaluation-queue .session)))))))

(defvar-local ob-julia-vterm-output-suppress-state nil)

(defun ob-julia-vterm-output-filter (str)
  "Remove the pasted julia code from STR."
  (let ((begin (string-match "#OB-JULIA-VTERM_BEGIN" str))
	(end (string-match "#OB-JULIA-VTERM_END" str))
	(state ob-julia-vterm-output-suppress-state))
    (if begin (setq ob-julia-vterm-output-suppress-state 'suppress))
    (if end (setq ob-julia-vterm-output-suppress-state nil))
    (let* ((str (replace-regexp-in-string
		 "#OB-JULIA-VTERM_BEGIN \\([0-9a-z]*\\)\\(.*?\n\\)*.*" "Executing... \\1\r\n" str))
	   (str (replace-regexp-in-string
		 "\\(.*?\n\\)*.*#OB-JULIA-VTERM_END" "" str)))
      (if (or begin end)
	  str
	(if state "" str)))))

(defun ob-julia-vterm-wait-for-file-change (file sec interval)
  "Wait up to SEC seconds synchronously until FILE becomes non-empty.
The file is checked at INTERVAL second intervals while waiting."
  (let ((c 0))
    (while (and (< c (/ sec interval))
		(= 0 (file-attribute-size (file-attributes file))))
      (sleep-for interval)
      (setq c (1+ c))))
  (sleep-for 0.1))

(defun ob-julia-vterm-process-one-evaluation-sync (session evaluation)
  "Execute the first EVALUATION in SESSION's queue synchronously.
Return the result."
  (with-current-buffer (julia-vterm-repl-buffer session)
    (while (not (eq (julia-vterm-repl-prompt-status) :julia))
      (message "Waiting REPL becomes ready")
      (sleep-for 0.1))
    (let-alist evaluation
      (julia-vterm-paste-string
       (ob-julia-vterm-make-str-to-run .uuid
				       .params
				       .src-file
				       .out-file
				       .stdin)
       .session)
      (ob-julia-vterm-wait-for-file-change .out-file 10 0.1)
      (with-temp-buffer
	(insert-file-contents .out-file)
	(buffer-string)))))

(defun ob-julia-vterm-process-one-evaluation-async (session)
  "Execute the first evaluation in SESSION's queue asynchronously.
Always return nil."
  (with-current-buffer (julia-vterm-repl-buffer session)
    (if (eq (julia-vterm-repl-prompt-status) :julia)
	(let-alist (queue-first ob-julia-vterm-evaluation-queue)
	  (unless (assoc .uuid ob-julia-vterm-evaluation-watches)
	    (let ((desc (file-notify-add-watch .out-file
					       '(change)
					       (ob-julia-vterm-evaluation-completed-callback-func session))))
	      (push (cons .uuid desc) ob-julia-vterm-evaluation-watches))
	    (julia-vterm-paste-string
	     (ob-julia-vterm-make-str-to-run .uuid
					     .params
					     .src-file
					     .out-file
					     .stdin)
	     .session)))
      (if (null ob-julia-vterm-evaluation-watches)
	  (run-at-time 0.1 nil #'ob-julia-vterm-process-evaluation-queue session))))
  nil)

(defun ob-julia-vterm-process-evaluation-queue (session)
  "Process the evaluation queue for SESSION.
If ASYNC is non-nil, the next evaluation will be executed asynchronously."
  (with-current-buffer (julia-vterm-repl-buffer session)
    (if (and (queue-p ob-julia-vterm-evaluation-queue)
	     (not (queue-empty ob-julia-vterm-evaluation-queue)))
	(ob-julia-vterm-process-one-evaluation-async session)
      (message "Queue empty"))))

(defun ob-julia-vterm-evaluate (buf session use-let body params)
  "Evaluate a Julia code block in BUF in a julia-vterm REPL specified with SESSION.
BODY contains the source code to be evaluated, and PARAMS contains header arguments.
With non-nil USE-LET, the code will be executed in a `let' block"
  (let* ((uuid (org-id-uuid))
	 (src-file (org-babel-temp-file "julia-vterm-src-"))
	 (out-file (org-babel-temp-file "julia-vterm-out-"))
	 (result-params (cdr (assq :result-params params)))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
		  (when stdin
		    (let ((tmp (org-babel-temp-file "julia-vterm-stdin-"))
			  (res (org-babel-ref-resolve stdin)))
		      (with-temp-file tmp
			(insert res))
		      tmp))))
	 (async (not (or (eq (org-element-type (org-element-context)) 'babel-call)
			 (member 'org-babel-ref-resolve (mapcar #'cadr (backtrace-frames)))))))
    (with-temp-file src-file (insert (ob-julia-vterm-wrap-body use-let body)))
    (let ((elm (org-element-context))
	  (src-block-begin (make-marker))
	  (src-block-end (make-marker)))
      (set-marker src-block-begin (org-element-property :begin elm))
      (set-marker src-block-end (org-element-property :end elm))
      (let ((evaluation (list (cons 'uuid uuid)
			      (cons 'async async)
			      (cons 'buf buf)
			      (cons 'session session)
			      (cons 'params params)
			      (cons 'src-file src-file)
			      (cons 'out-file out-file)
			      (cons 'stdin stdin)
			      (cons 'src-block-begin src-block-begin)
			      (cons 'src-block-end src-block-end))))
	(if (not async)
	    (ob-julia-vterm-process-one-evaluation-sync session evaluation)
	  (ob-julia-vterm-add-evaluation-to-evaluation-queue session evaluation)
	  (ob-julia-vterm-process-evaluation-queue session)
	  (concat "Executing... " (substring uuid 0 8)))))))

(add-to-list 'org-src-lang-modes '("julia-vterm" . julia))


;;----------------------------------------------------------------------
;; A helper minor mode for Org buffer with julia-vterm source code blocks.

(defun ob-julia-vterm-session-context ()
  "Return contextual information for determining which REPL to interact with.
The returned alist has three keys, block-ses, fellow-ses, and file-ses."
  (let* ((raw (org-element-property :parameters (org-element-at-point)))
	 (session-specified (string-match-p ":session" (or raw "")))
	 (src-block-info (org-babel-get-src-block-info))
	 (block-ses (and session-specified
                         (cdr (assoc :session (caddr src-block-info)))))
         (fellow-ses (and (buffer-live-p julia-vterm-fellow-repl-buffer)
                          (julia-vterm-repl-session-name julia-vterm-fellow-repl-buffer)))
         (props (org-entry-get-with-inheritance "header-args:julia"))
         (file-ses (cadr (member ":session" (split-string (or props ""))))))
    (list (cons 'block-ses block-ses)
          (cons 'fellow-ses fellow-ses)
          (cons 'file-ses file-ses))))

(defun ob-julia-vterm-session ()
  "Return the julia-vterm session name based on context."
  (let-alist (ob-julia-vterm-session-context)
    (if (or (null .block-ses)
            (string= .block-ses "none"))
        (or .fellow-ses
            (if (and .file-ses (not (string= .file-ses "none")))
                .file-ses
              "main"))
      .block-ses)))

(defun ob-julia-vterm-session-none-p ()
  "Return whether the block should be executed in let block."
  (let-alist (ob-julia-vterm-session-context)
    (if .block-ses
        (string= .block-ses "none")
      (string= .file-ses "none"))))

(defun ob-julia-vterm-fellow-repl-buffer (&optional session-name)
  "Return the paired REPL buffer for the current src block.
If SESSION-NAME is specified, return the REPL buffer for that session."
  (julia-vterm-repl-buffer (or session-name (ob-julia-vterm-session))))

(defun ob-julia-vterm-switch-to-repl-buffer (&optional arg)
  "Switch to the paired REPL buffer or to the one with a specified session.
With prefix ARG, prompt for session name."
  (interactive "P")
  (let ((session-name (cond ((null arg) nil)
                            (t (julia-vterm-ask-session)))))
    (julia-vterm-switch-to (ob-julia-vterm-fellow-repl-buffer session-name))))

(defun ob-julia-vterm-send-region-or-current-line ()
  "Send the content of the region if the region is active, or the current line."
  (interactive)
  (julia-vterm-send-region-or-current-line))

(defvar ob-julia-vterm-helper-mode-map
  (let ((map (copy-keymap julia-vterm-mode-map)))
    (define-key map (kbd "C-c C-z") #'ob-julia-vterm-switch-to-repl-buffer)
    (define-key map (kbd "C-<return>") #'ob-julia-vterm-send-region-or-current-line)
    (define-key map (kbd "C-c C-b") #'org-babel-execute-buffer)
    (define-key map (kbd "C-c C-i") nil t)
    map))

;;;###autoload
(define-minor-mode ob-julia-vterm-helper-mode
  "A minor mode for Org buffer with julia-vterm source code blocks."
  :init-value nil
  :lighter " ⁂"
  :keymap ob-julia-vterm-helper-mode-map
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot use `ob-julia-vterm-helper-mode' outside Org mode")))

(unless (fboundp 'julia-helper-mode)
  (defalias 'julia-helper-mode 'ob-julia-vterm-helper-mode))


(provide 'ob-julia-vterm)

;;; ob-julia-vterm.el ends here
