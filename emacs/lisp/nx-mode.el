;;; nx-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: July 20, 2024
;; Modified: July 20, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/nx-mode
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar nx--workspace-project-list-cache (make-hash-table :test 'equal)
  "Caches the list of nx projects in nx workspaces.

This is essentially a map from workspace roots to project names.")

(defvar nx--workspace-project-config-cache (make-hash-table :test 'equal)
  "Caches the project configurations in nx workspaces.")

(defvar nx-cmd "./node_modules/.bin/nx"
  "Path the the nx executable (relative to the workspace root).")

(defvar nx-workspace-root nil
  "Path the the nx workspace root.")

(defun nx--workspace-project-list-cache-clear-all ()
  "Clears all nx projects from `nx--project-list-cache'."
  (clrhash nx--workspace-project-list-cache))

(defun nx--workspace-project-list-cache-clear (workspace)
  "Clears the cached nx project list of WORKSPACE."
  (remhash (expand-file-name (concat workspace "/"))
           nx--workspace-project-list-cache))

(defun nx--workspace-project-list-cache-get (workspace)
  (gethash (expand-file-name (concat workspace "/"))
           nx--workspace-project-list-cache))

(defun nx-workspace-root (&optional workspace)
  "Get the absolute nx workspace root of WORKSPACE."
  (let* ((w (or workspace
                nx-workspace-root
                (project-root (project-current))))
         (wdir (expand-file-name (concat w "/"))))
    (when (nx--workspace-root-p wdir)
      wdir)))

(defun nx--workspace-load-projects (&optional workspace)
  "Gets the projects for nx workspace WORKSPACE."
  (when-let ((workspace-root (nx-workspace-root workspace)))
    (let* ((default-directory workspace-root)
           (cmdout (shell-command-to-string (concat nx-cmd " show projects")))
           (projects (split-string (substring cmdout 0 -1) "\n")))
      (puthash workspace-root projects nx--workspace-project-list-cache))))

(defun nx--workspace-projects (&optional workspace invalidate-case)
  "Gets the projects of the provided WORKSPACE.
With INVALIDATE-CACHE invalidates the cache first."
  (when-let ((workspace-root (nx-workspace-root workspace)))
    (when invalidate-case
      (remhash workspace-root nx--workspace-project-list-cache))
    (or (gethash workspace-root nx--workspace-project-list-cache)
        (nx--workspace-load-projects workspace-root))))

(defun nx--workspace-root-p (path)
  "Checks if PATH is an nx workspace root."
  (file-exists-p (expand-file-name "nx.json" path)))

(defun nx--workspace-p (project)
  "Checks whether PROJECT is an nx workspace."
  (nx--workspace-root-p (project-root project)))

(defun nx-clear-emacs-caches ()
  "Clears all nx caches."
  (interactive)
  (nx--workspace-project-list-cache-clear-all)
  (clrhash nx--workspace-project-config-cache))

(defun nx-known-workspace-roots ()
  "Lists the known nx workspace roots."
  (seq-filter #'nx--workspace-root-p (project-known-project-roots)))

(defun nx--workspace-projects-config-cache (workspace-root)
  "Gets the hash table for the provided WORKSPACE-ROOT"
  (or (gethash workspace-root
               nx--workspace-project-config-cache)
      (puthash workspace-root (make-hash-table :test 'equal)
               nx--workspace-project-config-cache)))

(defun nx--workspace-project-load-config (nx-project workspace-root)
  "Load the configuration of the provided nx project"
  (let* ((default-directory workspace-root)
         (config (json-parse-string
                  (shell-command-to-string (concat nx-cmd " show project " nx-project)))))
    (puthash nx-project config
             (nx--workspace-projects-config-cache workspace-root))))

(defun nx-project-config (nx-project &optional workspace invalidate-cache)
  "Gets the full config of NX-PROJECT.

With INVALIDATE-CACHE invalidates the cache first."
  (when-let ((workspace-root (nx-workspace-root workspace)))
    (when invalidate-cache
      (remhash nx-project
               (nx--workspace-projects-config-cache workspace-root)))
    (or (gethash nx-project
                 (nx--workspace-projects-config-cache workspace-root))
        (nx--workspace-project-load-config nx-project workspace-root))))

(defun nx-project-config-key (key nx-project &optional workspace invalidate-cache)
  "Gets the config key KEY of the NX-PROJECT configuration."
  (when-let ((config (nx-project-config nx-project workspace invalidate-cache)))
    (cond ((stringp key) (gethash key config))
          ((sequencep key) (cl-reduce (lambda (table k)
                                        (gethash k table))
                                      key
                                      :initial-value config))
          (t nil))))

(defun nx-project-root (nx-project &optional workspace invalidate-cache)
  "Gets the project root of NX-PROJECT"
  (nx-project-config-key "root" nx-project workspace invalidate-cache))

(defun nx-absolute-project-root (nx-project &optional workspace invalidate-cache)
  "Gets the absolute path to the NX-PROJECT root."
  (when-let ((workspace-root (nx-workspace-root workspace))
             (project-root (nx-project-root nx-project workspace invalidate-cache)))
    (concat workspace-root project-root "/")))

(defun nx-project-targets (nx-project &optional workspace invalidate-cache)
  "Gets the targets of NX-PROJECT"
  (when-let ((targets (nx-project-config-key "targets" nx-project workspace invalidate-cache)))
    (hash-table-keys targets)))

(defun nx-project-dired (nx-project)
  "Opens the NX-PROJECT root in dired."
  (interactive (list (completing-read "Select nx project: " (nx--workspace-projects))))
  (dired (nx-absolute-project-root nx-project)))

(defun nx-project-find-file (nx-project)
  "Opens find file in the project root of NX-PROJECT."
  (interactive (list (completing-read "Select nx project: " (nx--workspace-projects))))
  (let ((default-directory (nx-absolute-project-root nx-project)))
    (call-interactively 'find-file)))

(defun nx-project-run-target (nx-project &optional target)
  "Runs a target of NX-PROJECT."
  (interactive (list (completing-read "Select nx project: " (nx--workspace-projects))))
  (if-let* ((targets (nx-project-targets nx-project))
            (target (or target
                        (completing-read "Select nx target to run: " targets))))
      (projectile-run-async-shell-command-in-root
       (concat nx-cmd " run " nx-project ":" target)
       (concat "*nx run " nx-project ":" target "*"))
    (message (concat "No targets found for project " nx-project))))

(provide 'nx-mode)
;;; nx-mode.el ends here
