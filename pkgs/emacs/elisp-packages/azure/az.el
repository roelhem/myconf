;;; az.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <ik@roelweb.com>
;; Maintainer: Roel Hemerik <ik@roelweb.com>
;; Created: december 23, 2024
;; Modified: december 23, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roelhem/az
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar az-command "az"
  "Path to the az binary.")

(defun az--build-command (&rest args)
  "Builds an az command with ARGS."
  (cl-labels ((join (x)
                (pcase x
                  ((pred stringp) x)
                  ((pred numberp) (number-to-string x))
                  ((pred symbolp) (let ((xstr (symbol-name x)))
                                    (if (string-prefix-p ":" xstr)
                                        (concat (if (length= xstr 2) "-" "--")
                                                (substring xstr 1))
                                      xstr)))
                  ((pred sequencep) (mapconcat #'join x " "))
                  (_ (error "Invalid type `%s'" (type-of x))))))
    (join (cons az-command args))))

(defun az-command (&rest args)
  "Runs an az command."
  (shell-command (az--build-command args)))

(defun az-login (&rest args)
  "Run az login."
  (interactive)
  (az-command "login --allow-no-subscriptions" args))

(transient-define-prefix az-transient ()
  "Transient for the Azure az command line."
  :info-manual "The az command line"
  [("q" "Quit" transient-quit-one)])

(transient-define-argument az-transient--output-a ()
  "The output argument."
  :description "Output format."
  :class 'transient-option
  :shortarg "-o"
  :argument "--output="
  :choices '("json" "jsonc" "none" "table" "tsv" "yaml" "yamlc")
  :default "json")

(transient-define-prefix az-account ()
  "Transient for the Azure ~az account~ cli."
  :info-manual "az account"
  ["Global Arguments"
   ("-h" "Show help message and exit." "--help")
   (az-transient--output-a)
   ("-q" "JMESPath query string." "--query")]
  ["Commands"
   ("ss" "Show" transient-quit-one)])

(provide 'az)
;;; az.el ends here
