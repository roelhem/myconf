;;; tstools.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: October 25, 2024
;; Modified: October 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/tstools
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'treesit)

(defvar tstools--language-queries '((typescript)))

(defun tstools--prefix-symbol (prefix symbol)
  "Gives the prefixed SYMBOL with PREFIX.

Just gives SYMBOL if PREFIX is nil."
  (if (or (null prefix) (string-empty-p prefix)) symbol
    (intern (format "%s-%s" prefix symbol))))

(defun tstools-prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (if (or (null prefix) (string-empty-p prefix)) settings
    (mapcar (lambda (settings)
              (list (nth 0 settings)
                    (nth 1 settings)
                    (tstools--prefix-symbol prefix (nth 2 settings))
                    (nth 3 settings)))
            settings)))


(defun tstools-prefix-font-lock-feature-list (prefix features)
  "Prefix with PREFIX all symbols in FEATURES."
  (mapcar (lambda (fs) (mapcar (apply-partially #'tstools--prefix-symbol prefix) fs))
          features))

(defun tstools--merge-font-lock-feature-lists (lists)
  "Merge feature LISTS."
  (nreverse
   (named-let go ((ls lists)
                  (res nil))
     (if (cl-every #'null ls) res
       (go (mapcar #'cdr ls)
           (cons (apply #'append (mapcar #'car ls)) res))))))

(defun tstools-major-mode-font-lock-feature-list (mode)
  "Return the `treesit-font-lock-feature-list' of major MODE."
  (with-temp-buffer
    (funcall mode)
    treesit-font-lock-feature-list))

(defun tstools-major-mode-font-lock-settings (mode)
  "Return the `treesit-font-lock-settings' of major MODE."
  (with-temp-buffer
    (funcall mode)
    treesit-font-lock-settings))

(provide 'tstools)
;;; tstools.el ends here
