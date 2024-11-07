;;; lsp-bicep.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: October 17, 2024
;; Modified: October 17, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/lsp-bicep
;; Package-Requires: ((emacs "29.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'lsp-mode)

;; Configuration

(defgroup lsp-bicep nil
  "Customization group for `lsp-bicep'."
  :group 'lsp-mode)

;; Language server options
(defcustom lsp-bicep-langserver-path
  "Bicep.LangServer"
  "Path to the `Bicep.LangServer` binary"
  :group 'lsp-bicep
  :type 'file)

;; Register the client itself
(lsp-register-client
 (make-lsp--client
  :new-connection (lsp-stdio-connection (lambda () lsp-bicep-langserver-path))
  :major-modes '(bicep-ts-mode)
  :activation-fn (lsp-activate-on "bicep" "bicep-params")
  :server-id 'bicep-langserver))

(provide 'lsp-bicep)
;;; lsp-bicep.el ends here
