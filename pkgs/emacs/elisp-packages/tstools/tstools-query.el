;;; tstools-query.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Roel Hemerik
;;
;; Author: Roel Hemerik <roel@shared.nl>
;; Maintainer: Roel Hemerik <roel@shared.nl>
;; Created: October 23, 2024
;; Modified: October 23, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/roel/tstools-query
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

(define-derived-mode tstools-query-mode prog-mode "tstools-query-builder"
  "Major mode for building treesit queries and testing them live.")

(defconst tstools-query-builder-buffer-name "*tstools-query-builder*"
  "Name of the builder buffer.")

(defvar tstools-query--target-buffer nil
  "Target buffer to run the queries against.")

(defface tstools-query-match-1
  '((t :foreground "#000"
     :background "#00bfff"
     :weight bold))
  "Face for highlight captures in matches.")

(defface tstools-query-match-capture-label
  '((t :weight bold))
  "Face for the labels of matched captures.")

(defun tstools-query-set-target (buf)
  "Set the target BUF for tstools-query."
  (interactive "bTarget buffer: ")
  (if (bufferp tstools-query--target-buffer)
      (tstools-query-remove-highlights buf))
  (when-let ((buffer (get-buffer buf)))
    (setq tstools-query--target-buffer buffer)))

(defun tstools-query--target-node (&optional language)
  "Get the target node to query."
  (unless (bufferp tstools-query--target-buffer)
    (error "Target buffer is not a buffer"))
  (with-current-buffer tstools-query--target-buffer
    (treesit-buffer-root-node language)))

(defun tstools-query--highlight-capture (capture &optional face)
  "Highlight CAPTURE in the buffer if the captured node."
  (pcase-let* ((`(,capture-symbol . ,node) capture)
               (buffer (treesit-node-buffer node))
               (start (treesit-node-start node))
               (end (treesit-node-end node))
               (capture-name (symbol-name capture-symbol))
               (overlay (make-overlay start end buffer)))
    (unless (string-prefix-p "_" capture-name)
      (overlay-put overlay 'tstools-query-hl t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'face (or face 'tstools-query-match-1))
      (unless (string= capture-name "")
        (overlay-put overlay 'after-string (propertize (concat "@" capture-name)
                                                       'face 'tstools-query-match-capture-label))
        (overlay-put overlay 'help-echo capture-name)))))

(defun tstools-query-remove-highlights (&optional buf)
  "Remove all query highlight overlays from BUF.

Uses the current buffer if BUF is nil."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (remove-overlays nil nil 'tstools-query-hl t)))

(defun tstools-query--test-queries (queries &optional node)
  "Test QUERIES against NODE.

Uses the target query if NODE is nil."
  (let ((node (or node (tstools-query--target-node))))
    (unless (treesit-node-p node)
      (error "Invalid target node"))
    (tstools-query-remove-highlights (treesit-node-buffer node))
    (cl-loop for query in queries
             for captures = (treesit-query-capture node query)
             do (mapc #'tstools-query--highlight-capture captures)
             collect captures)))

(defmacro tstools-query-test (&rest args)
  "Testing tree sitter queries."
  `(tstools-query--test-queries (quote ,args)))

(provide 'tstools-query)
;;; tstools-query.el ends here
