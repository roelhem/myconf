;;; haskell-ts-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: January 11, 2024
;; Modified: January 11, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/haskell-ts-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require' treesit)

(defvar haskell-ts-font-lock-rules
  '(:language 'haskell
    :override t
    :feature 'delimiter
    '()

    :language 'haskell
    :feature 'delimiter
    '()

    :language 'haskell
    :feature 'type
    '())
  "Tree-sitter font-lock settings for haskell-ts-mode.")

(defvar haskell-ts-indent-rules
  '((haskell)))

(defun haskell-ts-setup ()
  "Setup treesit for haskell-ts-mode."

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     haskell-ts-font-lock-rules))

  (setq-local treesit-simple-indent-rules haskell-ts-indent-rules)

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (constant tag attribute)
                (declaration)
                (delimiter)))

  (treesit-major-mode-setup))

(define-derived-mode haskell-ts-mode prog-mode "Haskell (TS)"
  "Tree sitter haskell mode."
  (when (version< emacs-version "29.1")
    (error "The haskell-ts-mode requires at least Emacs 27.1"))

  (unless (treesit-ready-p 'haskell)
    (error "Tree-sitter for Haskell is not available"))

  (haskell-ts-setup))

(provide 'haskell-ts-mode)
;;; haskell-ts-mode.el ends here
