;;; arduino-ts-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <ik@roelweb.com>
;; Maintainer: Roel Hemerik <ik@roelweb.com>
;; Created: december 22, 2024
;; Modified: december 22, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roelhem/arduino-ts-mode
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
(require 'c-ts-mode)
(eval-when-compile (require 'rx))

(defgroup arduino-ts nil
  "Customize arduino-ts-mode."
  :prefix "arduino-ts-mode-"
  :group 'arduino)

(defcustom arduino-ts-mode-home nil
  "The arduino home directory."
  :type '(choice
          (directory :tag "Directory")
          (const :tag "Auto" nil))
  :group 'arduino-ts)

(defun arduino-ts-mode-toggle-comment-style (&optional arg)
  "Toggle the comment style between block and line comments.
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or left out."
  (interactive "P")
  (let ((prevstate-line (string= comment-start "// ")))
    (when (or (not arg)
              (zerop (setq arg (prefix-numeric-value arg)))
              (xor (> 0 arg) prevstate-line))
      (pcase-let ((`(,starter . ,ender)
                   (if prevstate-line
                       (cons "/* " " */")
                     (cons "// " ""))))
        (setq-local comment-start starter
                    comment-end ender))
      (arduino-ts-mode-set-modeline))))

(defun arduino-ts-mode-set-modeline ()
  "Set the mode line for arduino ts mode."
  (setq mode-name
        (concat "Arduino-ts"
                (string-trim-right comment-start)))
  (force-mode-line-update))

(defun arduino-ts-mode-home ()
  "Return the arduino-home-directory."
  (or arduino-ts-mode-home
      (pcase system-type
        ('darwin (file-truename "~/Documents/Arduino"))
        (_ (file-truename "~/Arduino")))))

(define-derived-mode arduino-ts-mode c++-ts-mode "Arduino-ts"
  "Major mode for editing Arduino sketch files."
  :group 'arduino-ts
  :after-hook (arduino-ts-mode-set-modeline))

(provide 'arduino-ts-mode)
;;; arduino-ts-mode.el ends here
