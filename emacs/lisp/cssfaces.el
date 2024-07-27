;;; cssfaces.el --- Description -*- lexical-binding: t; -*-
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

(require 'htmlize)

(defun cssfaces--face-valid-p (face)
  "Return non-nil if FACE can be converted to css."
  (and (facep face)
       (let ((inherit (face-attribute face :inherit)))
         (pcase inherit
           ((pred listp)
            (eval (cons 'and (mapcar #'cssfaces--face-valid-p inherit))))
           ('unspecified t)
           (_ (cssfaces--face-valid-p inherit))))))

(defun cssfaces--face-list ()
  "Return a list of faces that can be converted to css."
  (seq-filter #'cssfaces--face-valid-p (face-list)))


(defun cssfaces--write-faces (theme &optional faces)
  "Writes the FACES of theme THEME in css."
  (let* ((faces (or faces (cssfaces--face-list)))
         (htmlize-css-name-prefix "emacs-")
         (face-map (htmlize-make-face-map faces)))
    (insert (format "/*
Css generated from Emacs Font Faces of theme '%s'.

Generated at: %s
*/\n" theme (format-time-string "%Y-%m-%d %H:%M:%s" (current-time))))
    (insert (format "[data-theme=\"%s\"] {\n" theme))
    (dolist (face faces)
      (let ((fstruct (gethash face face-map)))
        (when (htmlize-fstruct-foreground fstruct)
          (insert (format "  --%s-fg: %s;\n"
                          (htmlize-fstruct-css-name fstruct)
                          (htmlize-fstruct-foreground fstruct))))

        (when (htmlize-fstruct-background fstruct)
          (insert (format "  --%s-bg: %s;\n"
                          (htmlize-fstruct-css-name fstruct)
                          (htmlize-fstruct-background fstruct))))))
    (insert "}\n\n")
    (insert "/* faces */\n")
    (dolist (face faces)
      (let* ((fstruct (gethash face face-map))
             (css-name (htmlize-fstruct-css-name fstruct)))
        (insert (format "/* source-face: %s */\n" face))
        (insert (format ".%s {\n" css-name))
        (when (htmlize-fstruct-foreground fstruct)
          (insert (format "  color: var(--%s-fg);\n" css-name)))
        (when (htmlize-fstruct-background fstruct)
          (insert (format "  background-color: var(--%s-bg);\n" css-name)))
        (let ((size (htmlize-fstruct-size fstruct)))
          (when size
            (cond ((floatp size)
                   (insert (format "  font-size: %srem;\n" size)))
                  (t
                   (insert (format "  font-size: %spt;\n" (/ size 10.0)))))))
        (when (htmlize-fstruct-boldp fstruct)
          (insert "  font-weight: bold;\n"))
        (when (htmlize-fstruct-italicp fstruct)
          (insert "  font-style: italic;\n"))
        (when (htmlize-fstruct-underlinep fstruct)
          (insert "  text-decoration: underline;\n"))
        (when (htmlize-fstruct-overlinep fstruct)
          (insert "  text-decoration: overline;\n"))
        (when (htmlize-fstruct-strikep fstruct)
          (insert "  text-decoration: line-through;\n"))
        (insert "}\n\n")))))

(defun cssfaces-export-faces (&optional file faces)
  "Exports FACES to a css FILE."
  (interactive)
  (let* ((file (or file "~/.myconf/emacs/resources/one-dark.css")))
    (find-file-other-window file)
    (erase-buffer)
    (cssfaces--write-faces "one-dark" faces)))

(provide 'cssfaces)
;;; cssfaces.el ends here
