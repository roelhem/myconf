;;; my-dashboard.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: January 11, 2024
;; Modified: January 11, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/my-dashboard
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar my-dashboard-buffer-name "*my-dashboard*"
  "The name to use for my dashboard buffer.")

(defvar my-dashboard-widgets
  '()
  "Lists widgets to render to construct my dashboard")

(defvar my-dashboard--last-cwd nil)
(defvar my-dashboard--width 80)
(defvar my-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar my-dashboard--pwd-alist ())

;;
;;; Bootstrap
(defun my-dashboard-init-h ()
  "Initializes my dashboard."
  (unless noninteractive
    (setq doom-fallback-buffer-name my-dashboard-buffer-name
          initial-buffer-choice #'doom-fallback-buffer)))

;;
;;; Faces
(defgroup my-dashboard nil
  "Manage how my dashboard is coloured and themed."
  :prefix "my-dashboard"
  :group 'custom)

(defface my-dashboard-banner '((t (:inherit font-lock-comment-face)))
  "Face used for my dashboard banner."
  :group 'my-dashboard)

(defface my-dashboard-footer '((t (:inherit font-lock-keyword-face)))
  "Face used for the footer on the dashboard"
  :group 'my-dashboard)


;;
;;; Major mode
(define-derived-mode my-dashboard-mode special-mode "Dashboard"
  "Major mode for my dashboard."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local display-line-numbers-type nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq-local fringe-indicator-alist alist))
  (setq-local hl-line-mode t))

(define-key! my-dashboard-mode-map
  [remap evil-delete]              #'ignore
  [remap evil-delete-line]         #'ignore
  [remap evil-insert]              #'ignore
  [remap evil-append]              #'ignore
  [remap evil-replace]             #'ignore
  [remap evil-enter-replace-state] #'ignore
  [remap evil-change]              #'ignore
  [remap evil-change-line]         #'ignore
  [remap evil-visual-char]         #'ignore
  [remap evil-visual-line]         #'ignore)

;;; Hooks
(defun my-dashboard-reposition-point-h ()
  "Traps the point in the buttons.")

(defun my-dashboard-reload-maybe-h (&rest _)
  "Reload the dashboard state.")

(defun my-dashboard-reload-frames-h (_frame)
  "Reload the dashboard")

(defun my-dashboard-resize-h (&rest _)
  "Recenter the dashboard, and reset its margins and fringes.")

(defun my-dashboard--persp-detect-project-h (&rest _)
  "Set dashboard's PWD to current perp's `last-project-root', if it exists.")

(defun my-dashboard--persp-record-project-h (&optional persp &rest _)
  "Record the last `doom-project-root' for the current persp.")
;;
;;; Library
(defun my-dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer my-dashboard-buffer-name)))

(defun my-dashboard-update-pwd-h (&optional pwd)
  "Update `default-directory' in my dashboard")

(defun my-dashboard-reload-on-theme-change-h ()
  "Forcibly reload my dashboard when theme changes post-startup."
  (when after-init-time
    (my-dashboard-reload 'force)))

(defun my-dashboard-reload (&optional force)
  "Update my dashboard (or create it, if it doesn't exist)"
  (when (or (and (get-buffer-window (doom-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-failure 'my-dashboard-inhibit-functions)))
            force)
    (with-current-buffer (doom-fallback-buffer)
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode 'my-dashboard-mode)
            (my-dashboard-mode))
          (erase-buffer)
          (run-hooks))))))

;;; Helpers
(defun my-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ?) s))

(defun my-dashboard--get-pwd ()
  (let ((lastcwd my-dashboard--last-cwd))
    (or (doom-project-root lastcwd)
        lastcwd)))

;;; Widgets

(defun my-dashboard-widget-banner ()
  (let ((point (point)))))


(provide 'my-dashboard)
;;; my-dashboard.el ends here
