;;; az-cli.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: January 13, 2024
;; Modified: January 13, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/az-cli
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require' transient)

(defvar az-cli-path "az"
  "Path to the az cli.")

(defun az-cli-to-string (cmd &rest args)
  "Performs an az call to CMD."
  (shell-command-to-string
   (string-join (cons az-cli-path (cons cmd args)) " ")))

(defvar az-cli--resource-group-names nil
  "List of the currently available resource groups.")

(defun az-cli--resource-group-names ()
  "Gets the list of current available resource group names. Fetches the if needed."
  (when (not az-cli--resource-group-names)
    (setq az-cli--resource-group-names (az-cli--fetch-resource-groups)))
  az-cli--resource-group-names)

(defun az-cli--fetch-resource-groups ()
  "Gets a list of the current available resource groups names."
  (process-lines az-cli-path "group" "list" "--query" "[].name" "--output" "tsv"))

(defun az-cli--fetch-storage-account-names (group)
  "Fetches a list of storage account names for GROUP."
  (process-lines az-cli-path "storage" "account" "list" "--resource-group" group "--output" "tsv" "--query" "[].name"))

(defun az-cli--fetch-storage-container-names (account)
  "Fetches a list of blob containers for ACCOUNT."
  (process-lines az-cli-path "storage" "container" "list" "--account-name" account "--auth-mode" "login" "--query" "[].name" "--output" "tsv"))

(defun az-cli-version ()
  "Get the az cli version."
  (interactive)
  (let ((buffer (get-buffer-create "*az-cli:version*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process az-cli-path
                    nil
                    buffer
                    nil
                    "version")
      (display-buffer buffer)
      (json-ts-mode))))



(transient-define-prefix az-cli-transient ()
  "Transient for the Azure az cli."
  :info-manual "az"
  ["Arguments"
   (az-cli--resource-group-list-a)]
  ["Other"
   ("V" "Shows the versions of Azure CLI modules and extensions in JSON format" az-cli-version)])

(transient-define-infix az-cli--resource-group-list-a ()
  :description "Resource Group"
  :class 'transient-option
  :shortarg "-g"
  :argument "--resource-group="
  :choices 'az-cli--resource-group-names)

(provide 'az-cli)
;;; az-cli.el ends here
