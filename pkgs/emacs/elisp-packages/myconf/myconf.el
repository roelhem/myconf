;;; myconf.el --- Tools to manage this configuration repository -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: October 23, 2024
;; Modified: October 23, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/myconf
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; Paths:
(require 'persp-mode)

(defvar myconf-dir "~/.myconf"
  "Directory where the myconf git-repository lives.")

(defun myconf--joindirs (root &rest paths)
  "Joins a list of PATHS relative to ROOT together."
  (if (not paths)
      root
    (apply 'myconf--joindirs
           (expand-file-name (car paths) root)
           (cdr paths))))

(defun myconf-dir (&rest args)
  "The resolved path to the myconf git-repository.

You may provide additional ARGS to get a subpath in the
myconf directory."
  (apply 'myconf--joindirs
         (file-name-as-directory (file-truename myconf-dir))
         args))

;;; Opening config files:

;; TODO: Implement dedicated frames.
(defvar myconf-config-open-other-frame nil
  "Whether to open the config literal file in a new frame.")

(defvar myconf-config-frame-name "Settings"
  "The name of the frame in which myconf should be opened.

Only has an effect if `myconf-config-open-other-frame' is non-nil.")

(defvar myconf-config-persp-name "*myconf*"
  "The name of the workspace in which the settings will be opened.

Only has an effect if `myconf-config-open-other-workspace' is non-nil.")

(defun myconf-persp-exists-p ()
  "Whether the myconf persp exists."
  (member myconf-config-persp-name (persp-names)))

(defun myconf-persp-p (&optional persp)
  "Return non-nil if PERSP is the myconf perspective.

Uses the current perspective if PERSP is nil."
  (let ((persp (or persp (get-current-persp))))
    (and (perspective-p persp)
         (string-equal (persp-name persp) myconf-config-persp-name))))

(defun myconf-persp-switch (&optional frame)
  "Switch to the myconf perspective.

When FRAME is provided, switch to the perspective in that frame."
  (interactive)
  (persp-switch myconf-config-persp-name frame))

(defun myconf-open-config-org ()
  "Open the myconf literal file."
  (interactive)
  (myconf-persp-switch)
  (find-file (myconf-dir "config.org")))

(provide 'myconf)
;;; myconf.el ends here
