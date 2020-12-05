;;; ob-julia-vterm.el --- Babel Fucntions for Julia in VTerm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: October 31, 2020
;; URL: https://github.com/shg/ob-julia-vterm.el
;; Package-Requires: ((emacs "26.1") (julia-vterm "0.10"))
;; Version: 0.1
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

;;; Usage:

;; This package uses julia-vterm to run Julia code.  You also need to
;; have Suppressor.jl package installed in your Julia environment.
;;
;; Install ob-julia-vterm.el manually using package.el
;;
;;   (package-install-file "/path-to-download-dir/ob-julia-vterm.el")
;;
;; Now you can execute Julia source code blocks in org files.

;;; Code:

(require 'ob)
(require 'julia-vterm)

(defvar org-babel-julia-vterm-debug nil)

(defun org-babel-julia-vterm--wrap-body (result-type session body)
  "Make Julia code that execute-s BODY and obtains the results, according to RESULT-TYPE and SESSION."
  (concat
   "_julia_vterm_output = "
   (if (eq result-type 'output)
       (concat "@capture_out begin "
	       (if session "eval(Meta.parse(raw\"\"\"begin\n" "\n"))
     (if session "begin\n" "let\n"))
   body
   (if (and (eq result-type 'output) session)
       "\nend\"\"\"))")
   "\nend\n"))

(defun org-babel-julia-vterm--make-str-to-run (result-type src-file out-file)
  "Make Julia code that load-s SRC-FILE and saves the result to OUT-FILE, according to RESULT-TYPE."
  (format
   (concat
    (if (eq result-type 'output) "using Suppressor; ")
    "include(\"%s\");  open(\"%s\", \"w\") do file; print(file, _julia_vterm_output); end\n")
   src-file out-file))

(unless (fboundp 'org-babel-execute:julia)
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm))

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

(defun org-babel-julia-vterm-evaluate (session body result-type params)
  "Evaluate BODY as Julia code in a julia-vterm buffer specified with SESSION."
  (let ((src-file (org-babel-temp-file "julia-vterm-src-"))
	(out-file (org-babel-temp-file "julia-vterm-out-"))
	(src (org-babel-julia-vterm--wrap-body result-type session body)))
    (with-temp-file src-file (insert src))
    (when org-babel-julia-vterm-debug
      (julia-vterm-paste-string
       (format "#= params ======\n%s\n== src =========\n%s===============#\n" params src)
       session))
    (julia-vterm-paste-string
     (org-babel-julia-vterm--make-str-to-run result-type src-file out-file)
     session)
    (let ((c 0))
      (while (and (< c 50) (= 0 (file-attribute-size (file-attributes out-file))))
	(sit-for 0.1)
	(setq c (1+ c))))
    (let ((result (with-temp-buffer (insert-file-contents out-file) (buffer-string))))
      result)))

(add-to-list 'org-src-lang-modes '("julia-vterm" . "julia"))

(provide 'ob-julia-vterm)

;;; ob-julia-vterm.el ends here
