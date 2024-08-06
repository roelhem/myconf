;;; builder.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: August 02, 2024
;; Modified: August 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/elisp-nix-builder
;; Package-Requires: ((emacs "24.9"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun builder-attrs (&optional env)
  "Return the attributes passed to the builder.

Possibly set the ENV to change the input environment
variable name."
  (let ((input (getenv (or env "elispAttrs"))))
    (read (or input "nil"))))

(defun builder--eval-in-context (forms)
  "Evaluate all as FORMS in the builder context."
  (let* ((letattrs (mapcar
                    (lambda(x) "" (list (car x) (cdr x)))
                    (builder-attrs)))
         (expr `(let ((out (getenv "out"))
                      (default-directory (getenv "out"))
                      (system (getenv "system"))
                      (name (getenv "name"))
                      (builder (getenv "builder"))
                      (nix-store (getenv "NIX_STORE"))
                      . ,letattrs)
                  . ,forms)))
    (eval expr)))

(defmacro builder-context (&rest forms)
  "Run all FORMS in the builder context."
  `(builder--eval-in-context (quote ,forms)))

(defun builder--print-exec (expr)
  "Evaluate the forms of EXPR and prints the results."
  (dolist (parsed-form (read (concat "(" expr ")")))
    (let ((executable-form (if (listp parsed-form)
                               parsed-form
                             (list 'load (symbol-name parsed-form)))))
      (princ "\n=> ")
      (pp executable-form)
      (princ "\n")
      (pp (eval executable-form))
      (princ "\n\n"))))

(defun builder-exec-and-exit ()
  "Evaluate the first form of the expr in the builder context."
  (builder-context
   (builder--print-exec expr))
  (kill-emacs 0))

(defmacro builder-require (&rest features)
  "Require FEATURES for a nix-builder context.
Looks in the env variables if features are not found."
  (let ((forms (mapcar
                (lambda (feature)
                  `(require
                    (quote ,feature)
                    (getenv ,(symbol-name feature))))
                features)))
    `(progn . ,forms)))


(defun builder-exec-and-exit-with-buffer ()
  "Evaluate the forms of the expr attr in the builder context.

Also opens a temporary buffer to which those expressions can write.
Finally, it will write everything to the specified outFile."
  (builder-context
   (with-temp-buffer
     (builder--print-exec expr)
     (write-file out)))
  (kill-emacs 0))

(defun builder--org-tangle-file (src out &optional target-file lang-re keep-src-copy)
  "Tangle an org file in the SRC attribute to the OUT dir.

Optionally set TARGET-FILE to the default output file.
Filter the languages to tangle by setting LANG-RE to
a regular expression that matches only those languages.

The the SRC file will first be copied to the OUT dir
before tangling. This file will be deleted afterwards,
unless KEEP-SRC-COPY is non-nil."
  (builder-require ox ob-tangle)
  (make-directory out t)
  (let* ((src-copy (expand-file-name
                    (file-name-nondirectory src)))
         (default-directory out))
    (copy-file src src-copy)
    (with-current-buffer (find-file-noselect src-copy)
      (org-export-expand-include-keyword)
      (org-babel-tangle nil target-file lang-re))
    (unless keep-src-copy
      (delete-file src-copy))))

(defun builder-org-tangle-file ()
  "Tangle an org file in a nix-builder context."
  (builder-context
   (builder--org-tangle-file src out targetFile langRegex))
  (kill-emacs 0))

(provide 'builder)
;;; builder.el ends here
