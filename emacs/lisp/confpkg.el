;;; confpkg.el --- Builds the config from the org-file-*- lexical-binding: t; -*-
;;
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: July 29, 2023
;; Modified: July 29, 2023
;; Version: 0.0.1
;; Homepage: https://github.com/roel/confpkg
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Inspired by: https://tecosaur.github.io/emacs-config/config.html
;;
;;  Description
;;
;;; Code:

;; CONFIGURATION

;; Default values.
(defcustom confpkg-subconf-dir "subconf"
  "Directory in which the subconf should be installed"
  :type '(string)
  :group 'confpkg)

;; GLOBAL STATE

(defvar confpkg--num 0)
(defvar confpkg--list nil)


;; HELPERS


;; Indentifying cross-package dependencies
(defun confpkg--rough-extract-definitions (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (symbols)
      (while (re-search-forward
              (rx line-start (* (any ?\s ?\t)) "("
                  (or "defun" "defmacro" "defsubst" "defgeneric" "defalias" "defvar" "defcustom" "defface" "deftheme"
                      "cl-defun" "cl-defmacro" "cl-defsubst" "cl-defmethod" "cl-defstruct" "cl-defgeneric" "cl-deftype")
                  (+ (any ?\s ?\t))
                  (group (+ (any "A-Z" "a-z" "0-9"
                                 ?+ ?- ?* ?/ ?_ ?~ ?! ?@ ?$ ?% ?^ ?& ?= ?: ?< ?> ?{ ?})))
                  (or blank ?\n))
              nil t)
        (push (match-string 1) symbols))
      symbols)))

(defun confpkg--rough-uses-p (file symbols)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((symbols (copy-sequence symbols)) uses-p)
      (while symbols
        (goto-char (point-min))
        (if (re-search-forward (rx word-start (literal (car symbols)) word-end) nil t)
            (setq uses-p t symbols nil)
          (setq symbols (cdr symbols))))
      uses-p)))

(defun confpkg-annotate-list-dependencies ()
  (dolist (confpkg confpkg--list)
    (plist-put confpkg :defines
               (confpkg--rough-extract-definitions
                (plist-get confpkg :file))))
  (dolist (confpkg confpkg--list)
    (let ((after (plist-get confpkg :after))
          requires)
      (dolist (other-confpkg confpkg--list)
        (when (and (not (eq other-confpkg confpkg))
                   (confpkg--rough-uses-p (plist-get confpkg :file)
                                          (plist-get other-confpkg :defines)))
          (push (plist-get other-confpkg :package) requires)))
      (when (and after (symbolp after))
        (push after requires))
      (plist-put confpkg :requires requires))))

(defun confpkg-write-dependencies ()
  (dolist (confpkg confpkg--list)
    (when (plist-get confpkg :requires)
      (with-temp-buffer
        (setq buffer-file-name (plist-get confpkg :file))
        (insert-file-contents buffer-file-name)
        (re-search-forward "^;;; Code:\n")
        (insert "\n")
        (dolist (req (plist-get confpkg :requires))
          (insert (format "(require '%s)\n" req)))
        (write-region nil nil buffer-file-name)
        (set-buffer-modified-p nil)))))

(defun confpkg-comment-out-package-statements ()
  (dolist (confpkg confpkg--list)
    (with-temp-buffer
      (setq buffer-file-name (plist-get confpkg :file))
      (insert-file-contents buffer-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^;;; Code:\n[[:space:]\n]*(\\(package!\\|unpin!\\)[[:space:]\n]+\\([^[:space:]]+\\)\\b" nil t)
        (plist-put confpkg :package-statements
                   (nconc (plist-get confpkg :package-statements)
                          (list (match-string 2))))
        (let* ((start (progn (beginning-of-line) (point)))
               (end (progn (forward-sexp 1)
                           (if (looking-at "[\t ]*;.*")
                               (line-end-position)
                             (point))))
               (contents (buffer-substring start end))
               paste-start paste-end
               (comment-start ";")
               (comment-padding "   ")
               (comment-end ""))
          (delete-region start (1+ end))
          (re-search-backward "^;;; Code:")
          (beginning-of-line)
          (insert ";;  Package statement:\n")
          (setq paste-start (point))
          (insert contents)
          (setq paste-end (point))
          (insert  "\n;;\n")
          (comment-region paste-start paste-end 2)))
      (when (buffer-modified-p)
        (write-region nil nil buffer-file-name)
        (set-buffer-modified-p nil)))))

(defun confpkg-create-config ()
  (let ((revert-without-query '("config\\.el"))
        (keywords (org-collect-keywords '("AUTHOR" "EMAIL")))
        (original-buffer (current-buffer)))
    (with-temp-buffer
      (insert
       (format ";;; config.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: © 2020-%s %s <%s>
;; SPDX-License-Identifier: MIT

;; Generated at %s from the literate configuration.

(add-to-list 'load-path %S)\n"
               (format-time-string "%Y")
               (cadr (assoc "AUTHOR" keywords))
               (cadr (assoc "EMAIL" keywords))
               (format-time-string "%FT%T%z")
               (replace-regexp-in-string
                (regexp-quote (getenv "HOME")) "~"
                (expand-file-name "subconf/"))))
      (mapc
       (lambda (confpkg)
         (insert
          (if (eq 'none (plist-get confpkg :via))
              (format "\n;;; %s intentionally omitted.\n" (plist-get confpkg :name))
            (with-temp-buffer
              (cond
               ((eq 'copy (plist-get confpkg :via))
                (insert-file-contents (plist-get confpkg :file))
                (goto-char (point-min))
                (narrow-to-region
                 (re-search-forward "^;;; Code:\n+")
                 (progn
                   (goto-char (point-max))
                   (re-search-backward (format "[^\n\t ][\n\t ]*\n[\t ]*(provide '%s)" (plist-get confpkg :package)))
                   (match-end 0))))
               ((eq 'require (plist-get confpkg :via))
                (insert (format "(require '%s)\n" (plist-get confpkg :package))))
               (t (insert (format "(warn \"%s confpkg :via has unrecognised value: %S\" %S %S)"
                                  (plist-get confpkg :name) (plist-get confpkg :via)))))
              (goto-char (point-min))
              (insert "\n;;:------------------------"
                      "\n;;; " (plist-get confpkg :name)
                      "\n;;:------------------------\n\n")
              (when (plist-get confpkg :defines)
                (insert ";; This block defines "
                        (mapconcat
                         (lambda (d) (format "`%s'" d))
                         (plist-get confpkg :defines)
                         ", ")
                        ".")
                (when (re-search-backward "\\([^, ]+\\), \\([^, ]+\\), \\([^, ]+\\).\\="
                                          (line-beginning-position) t)
                  (replace-match "\\1, \\2, and \\3."))
                (when (re-search-backward "\\([^, ]+\\), \\([^, ]+\\).\\="
                                          (line-beginning-position) t)
                  (replace-match "\\1 and \\2."))
                (insert "\n\n")
                (forward-line -2)
                (setq-local comment-start ";")
                (fill-comment-paragraph)
                (forward-paragraph 1)
                (forward-line 1))
              (if (equal (plist-get confpkg :package) "config-confpkg-timings")
                  (progn
                    (goto-char (point-max))
                    (insert "\n\n\
(confpkg-create-record 'doom-pre-config (float-time (time-subtract (current-time) before-init-time)))
(confpkg-start-record 'config)
(confpkg-create-record 'config-defered 0.0 'config)
(confpkg-create-record 'set-hooks 0.0 'config-defered)
(confpkg-create-record 'load-hooks 0.0 'config-defered)
(confpkg-create-record 'requires 0.0 'root)\n"))
                (let ((after (plist-get confpkg :after))
                      (pre (and (plist-get confpkg :pre)
                                (org-babel-expand-noweb-references
                                 (list "emacs-lisp"
                                       (format "<<%s>>" (plist-get confpkg :pre))
                                       '((:noweb . "yes")
                                         (:comments . "none")))
                                 original-buffer)))
                      (name (replace-regexp-in-string
                             "config--?" ""
                             (plist-get confpkg :package))))
                  (if after
                      (insert (format "(confpkg-with-record '%S\n"
                                      (list (concat "hook: " name) 'set-hooks))
                              (if pre
                                  (concat ";; Begin pre\n" pre "\n;; End pre\n")
                                "")
                              (format (if (symbolp after) ; If single feature.
                                          "  (with-eval-after-load '%s\n"
                                        "  (after! %s\n")
                                      after))
                    (when pre
                      (insert "\n;; Begin pre (unnecesary since after is unused)\n"
                              pre
                              "\n;; End pre\n")))
                  (insert
                   (format "(confpkg-with-record '%S\n"
                           (list (concat "load: " name)
                                 (if after 'load-hooks 'config)))))
                (goto-char (point-max))
                (when (string-match-p ";" (thing-at-point 'line))
                  (insert "\n"))
                (insert ")")
                (when (plist-get confpkg :after)
                  (insert "))"))
                (insert "\n"))
              (buffer-string)))))
       (let ((confpkg-timings ;; Ensure timings is put first.
              (cl-some (lambda (p) (and (equal (plist-get p :package) "config-confpkg-timings") p))
                       confpkg--list)))
         (append (list confpkg-timings)
                 (nreverse (remove confpkg-timings confpkg--list)))))
      (insert "\n(confpkg-finish-record 'config)\n\n;;; config.el ends here")
      (write-region nil nil "config.el" nil :silent))))


;; CLEARING/INITIALIZING

(defun confpkg-clear ()
  "Initializes the global state values that keep track of the saved files"
  (setq confpkg--num 0
        confpkg--list nil))


(defun confpkg-clear-old-files ()
  "Removes all config files in =confpkg-subconf-dir="
  (make-directory confpkg-subconf-dir t)
  (dolist (conf-file (directory-files confpkg-subconf-dir t "config-.*\\.el"))
    (delete-file conf-file)))

;; TODO: Check how to call this
(defun confpkg-cleanup ()
  "Cleans up the confpkg residu"
  (org-fold-core-ignore-fragility-checks
  (org-babel-map-executables nil
    (when (and (eq (org-element-type (org-element-context)) 'babel-call)
               (equal (org-element-property :call (org-element-context)) "confpkg"))
      (org-babel-remove-result)
      (org-entry-delete nil "header-args:emacs-lisp")))))

;; Initialize an org page.
(defun confpkg-prepare ()
  "Prepare the babel page to be used for configuring."
  (condition-case nil
    (progn
      (message "Initializing confpkg...")
      (org-fold-core-ignore-fragility-checks
        (org-babel-map-executables nil
          (when (eq (org-element-type (org-element-context)) 'babel-call)
             (org-babel-lob-execute-maybe)))))
    (quit (revert-buffer t t t))))

;; LOAD TIME REPORTING

(defvar confpkg-load-time-tree (list (list 'root)))
(defvar confpkg-record-branch (list 'root))
(defvar confpkg-record-num 0)

(defun confpkg-create-record (name elapsed &optional parent enclosing)
  (let ((parent (assoc (or parent (car confpkg-record-branch))
                       confpkg-load-time-tree))
        (record (cons name (list (list 'self
                                       :name (format "%s" name)
                                       :num (cl-incf confpkg-record-num)
                                       :elapsed elapsed
                                       :enclosing enclosing)))))
    (push record confpkg-load-time-tree)
    (push record (cdr parent))
    record))

(defun confpkg-start-record (name &optional parent)
  (let ((record (confpkg-create-record name 0.0e+NaN parent t)))
    (plist-put (cdadr record) :start (float-time))
    (push name confpkg-record-branch)
    record))

(defun confpkg-finish-record (name)
  (let ((self-record (cdar (last (cdr (assoc name confpkg-load-time-tree))))))
    (plist-put self-record :elapsed
               (- (float-time) (plist-get self-record :start) 0.0))
    (unless (equal (car confpkg-record-branch) name)
      (message "Warning: Confpkg timing record expected to finish %S, instead found %S. %S"
               name (car confpkg-record-branch) confpkg-record-branch))
    (setq confpkg-record-branch (cdr confpkg-record-branch))))

(defmacro confpkg-with-record (name &rest body)
  "Create a time record around BODY.
The record must have a NAME."
  (declare (indent 1))
  (let ((name-val (make-symbol "name-val"))
        (record-spec (make-symbol "record-spec")))
    `(let* ((,name-val ,name)
            (,record-spec (if (consp ,name-val) ,name-val (list ,name-val))))
       (apply #'confpkg-start-record ,record-spec)
       (unwind-protect
           (progn ,@body)
         (confpkg-finish-record (car ,record-spec))))))

(defadvice! +require--log-timing-a (orig-fn feature &optional filename noerror)
  :around #'require
  (if (or (featurep feature)
          (eq feature 'cus-start) ; HACK Why!?!
          (assoc (format "require: %s" feature) confpkg-load-time-tree))
      (funcall orig-fn feature filename noerror)
    (confpkg-with-record (list (format "require: %s" feature)
                               (and (eq (car confpkg-record-branch) 'root)
                                    'requires))
      (funcall orig-fn feature filename noerror))))

(defun confpkg-timings-report (&optional sort-p node)
  "Display a report on load-time information.
Supply SORT-P (or the universal argument) to sort the results.
NODE defaults to the root node."
  (interactive
   (list (and current-prefix-arg t)))
  (let ((buf (get-buffer-create "*Confpkg Load Time Report*"))
        (depth 0)
        num-pad name-pad max-time max-total-time max-depth)
    (cl-labels
        ((sort-records-by-time
          (record)
          (let ((self (assoc 'self record)))
            (append (list self)
                    (sort (nreverse (remove self (cdr record)))
                          (lambda (a b)
                            (> (or (plist-get (alist-get 'self a) :total) 0.0)
                               (or (plist-get (alist-get 'self b) :total) 0.0)))))))
         (print-record
          (record)
          (cond
           ((eq (car record) 'self)
            (insert
             (propertize
              (string-pad (number-to-string (plist-get (cdr record) :num)) num-pad)
              'face 'font-lock-keyword-face)
             " "
             (propertize
              (apply #'concat
                     (make-list (1- depth) "• "))
              'face 'font-lock-comment-face)
             (string-pad (format "%s" (plist-get (cdr record) :name)) name-pad)
             (make-string (* (- max-depth depth) 2) ?\s)
             (propertize
              (format "%.4fs" (plist-get (cdr record) :elapsed))
              'face
              (list :foreground
                    (doom-blend 'orange 'green
                                (/ (plist-get (cdr record) :elapsed) max-time))))
             (if (= (plist-get (cdr record) :elapsed)
                    (plist-get (cdr record) :total))
                 ""
               (concat "   (Σ="
                       (propertize
                        (format "%.3fs" (plist-get (cdr record) :total))
                        'face
                        (list :foreground
                              (doom-blend 'orange 'green
                                          (/ (plist-get (cdr record) :total) max-total-time))))
                       ")"))
             "\n"))
           (t
            (cl-incf depth)
            (mapc
             #'print-record
             (if sort-p
                 (sort-records-by-time record)
               (reverse (cdr record))))
            (cl-decf depth))))
         (flatten-records
          (records)
          (if (eq (car records) 'self)
              (list records)
            (mapcan
             #'flatten-records
             (reverse (cdr records)))))
         (tree-depth
          (records &optional depth)
          (if (eq (car records) 'self)
              (or depth 0)
            (1+ (cl-reduce #'max (cdr records) :key #'tree-depth))))
         (mapreduceprop
          (list map reduce prop)
          (cl-reduce
           reduce list
           :key
           (lambda (p) (funcall map (plist-get (cdr p) prop)))))
         (elaborate-timings
          (record)
          (if (eq (car record) 'self)
              (plist-get (cdr record) :elapsed)
            (let ((total (cl-reduce #'+ (cdr record)
                                    :key #'elaborate-timings))
                  (self (cdr (assoc 'self record))))
              (if (plist-get self :enclosing)
                  (prog1
                      (plist-get self :elapsed)
                    (plist-put self :total (plist-get self :elapsed))
                    (plist-put self :elapsed
                               (- (* 2 (plist-get self :elapsed)) total)))
                (plist-put self :total total)
                total))))
         (elaborated-timings
          (record)
          (let ((record (copy-tree record)))
            (elaborate-timings record)
            record)))
      (let* ((tree
              (elaborated-timings
               (append '(root)
                       (copy-tree
                        (alist-get (or node 'root)
                                   confpkg-load-time-tree
                                   nil nil #'equal))
                       '((self :num 0 :elapsed 0)))))
             (flat-records
              (cl-remove-if
               (lambda (rec) (= (plist-get (cdr rec) :num) 0))
               (flatten-records tree))))
        (setq max-time (mapreduceprop flat-records #'identity #'max :elapsed)
              max-total-time (mapreduceprop flat-records #'identity #'max :total)
              name-pad (mapreduceprop flat-records #'length #'max :name)
              num-pad (mapreduceprop flat-records
                                     (lambda (n) (length (number-to-string n)))
                                     #'max :num)
              max-depth (tree-depth tree))
        (with-current-buffer buf
          (erase-buffer)
          (setq-local outline-regexp "[0-9]+ *\\(?:• \\)*")
          (outline-minor-mode 1)
          (use-local-map (make-sparse-keymap))
          (local-set-key "TAB" #'outline-toggle-children)
          (local-set-key "\t" #'outline-toggle-children)
          (local-set-key (kbd "<backtab>") #'outline-show-subtree)
          (local-set-key (kbd "C-<iso-lefttab>")
                         (eval `(cmd! (if current-prefix-arg
                                          (outline-show-all)
                                        (outline-hide-sublevels (+ ,num-pad 2))))))
          (insert
           (propertize
            (concat (string-pad "#" num-pad) " "
                    (string-pad "Confpkg"
                                (+ name-pad (* 2 max-depth) -3))
                    (format " Load Time (Σ=%.3fs)\n"
                            (plist-get (cdr (assoc 'self tree)) :total)))
            'face '(:inherit (tab-bar-tab bold) :extend t :underline t)))
          (dolist (record (if sort-p
                              (sort-records-by-time tree)
                            (reverse (cdr tree))))
            (unless (eq (car record) 'self)
              (print-record record)))
          (set-buffer-modified-p nil)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

;; SETUP

;; METHODS

;; Main function confpkg
(defun confpkg (name needs after pre prefix via)
  "For call"
  ())

;; FINALIZING

(defun confpkg-tangle-finalise ()
  (remove-hook 'org-babel-tangle-finished-hook #'confpkg-tangle-finalise)
  (revert-buffer t t t)
  (confpkg-comment-out-package-statements)
  (confpkg-annotate-list-dependencies)
  (confpkg-create-config)
  (confpkg-write-dependencies)
  (message "Processed %s elisp files" (length confpkg--list)))



(provide 'confpkg)
;;; confpkg.el ends here
