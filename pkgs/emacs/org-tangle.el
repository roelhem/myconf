;;; bin/org-tangle

;; Tangles source blocks from org files. Also expands #+INCLUDE directives,
;; unlike vanilla `ob-tangle'. Debug/info messages are directed to stderr and
;; can be ignored.
;;
;;   -l/--lang LANG
;;     Only include blocks in the specified language (e.g. emacs-lisp).
;;   -a/--all
;;     Tangle all blocks by default (unless it has :tangle nil set or a
;;     :notangle: tag)
;;   -t/--tag TAG
;;      --and TAG
;;      --or TAG
;;     Only include blocks in trees that have these tags. Combine multiple --and
;;     and --or's, or just use --tag (implicit --and).
;;   -p/--print
;;     Prints tangled code to stdout instead of to files
;;
;; Usage: org-tangle [[-l|--lang] LANG] some-file.org another.org
;; Examples:
;;   org-tangle -l sh modules/some/module/README.org > install_module.sh
;;   org-tangle -l sh modules/lang/go/README.org | sh
;;   org-tangle --and tagA --and tagB my/literate/config.org
;;
;; Source: doomemacs

(require 'cl-lib)
(require 'ox)
(require 'ob-tangle)

(defun usage ()
  (with-temp-buffer
    (insert (format "%s %s [OPTIONS] [TARGETS...]\n"
                    "[1mUsage:[0m"
                    (file-name-nondirectory load-file-name))
            "\n"
            "A command line interface for tangling org-mode files. TARGETS can be\n"
            "files or folders (which are searched for org files recursively).\n"
            "\n"
            "This is useful for literate configs that rely on command line\n"
            "workflows to build it.\n"
            "\n"
            "[1mExample:[0m\n"
            "  org-tangle some-file.org\n"
            "  org-tangle literate/config/\n"
            "  org-tangle -p -l sh scripts.org > do_something.sh\n"
            "  org-tangle -p -l python -t tagA -t tagB file.org | python\n"
            "\n"
            "[1mOptions:[0m\n"
            "  -a --all\t\tTangle all blocks by default\n"
            "  -l --lang LANG\tOnly tangle blocks written in LANG\n"
            "  -p --print\t\tPrint tangled output to stdout than to files\n"
            "  -t --tag TAG\n"
            "     --and TAG\n"
            "     --or TAG\n"
            "    Lets you tangle org blocks by tag. You may have more than one\n"
            "    of these options.\n")
    (princ (buffer-string))))

(defun *org-babel-tangle (fn &rest args)
  "Don't write tangled blocks to files, print them to stdout."
  (cl-letf (((symbol-function 'write-region)
             (lambda (start end filename &optional append visit lockname mustbenew)
               (princ (buffer-string)))))
    (apply fn args)))

(defun *org-babel-tangle-collect-blocks (&optional language tangle-file)
  "Like `org-babel-tangle-collect-blocks', but will ignore blocks that are in
trees with the :notangle: tag."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
             (org-with-wide-buffer
              (org-with-limited-levels (outline-previous-heading)))))
        (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
          (setq counter 1)
          (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
        (require 'org)
        (let* ((tags (org-get-tags-at))
               (info (org-babel-get-src-block-info 'light))
               (src-lang (nth 0 info))
               (src-tfile (cdr (assq :tangle (nth 2 info)))))
          (cond ((member "notangle" tags))

                ((and (or or-tags and-tags)
                      (or (not and-tags)
                          (let ((a (cl-intersection and-tags tags :test #'string=))
                                (b and-tags))
                            (not (or (cl-set-difference a b :test #'equal)
                                     (cl-set-difference b a :test #'equal)))))
                      (or (not or-tags)
                          (cl-intersection or-tags tags :test #'string=))
                      t))

                ((or (not (or all-blocks src-tfile))
                     (string= src-tfile "no")  ; tangle blocks by default
                     (and tangle-file (not (equal tangle-file src-tfile)))
                     (and language (not (string= language src-lang)))))

                ;; Add the spec for this block to blocks under its language.
                ((let ((by-lang (assoc src-lang blocks))
                       (block (org-babel-tangle-single-block counter)))
                   (if by-lang
                       (setcdr by-lang (cons block (cdr by-lang)))
                     (push (cons src-lang (list block)) blocks))))))))
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))
(advice-add #'org-babel-tangle-collect-blocks
            :override #'*org-babel-tangle-collect-blocks)

(defvar all-blocks nil)
(defvar and-tags nil)
(defvar or-tags nil)
(let (lang srcs and-tags or-tags)
  (pop argv)
  (while argv
    (let ((arg (pop argv)))
      (pcase arg
        ((or "-h" "--help")
         (usage)
         (error ""))
        ((or "-a" "--all")
         (setq all-blocks t))
        ((or "-l" "--lang")
         (setq lang (pop argv)))
        ((or "-p" "--print")
         (advice-add #'org-babel-tangle :around #'*org-babel-tangle))
        ((or "-t" "--tag" "--and")
         (push (pop argv) and-tags))
        ("--or"
         (push (pop argv) or-tags))
        ((guard (string-match-p "^--lang=" arg))
         (setq lang (cadr (split-string arg "=" t t))))
        ((guard (file-directory-p arg))
         (setq srcs
               (append (directory-files-recursively arg "\\.org$")
                       srcs)))
        ((guard (file-exists-p arg))
         (push arg srcs))
        (_ (error "Unknown option or file: %s" arg)))))

  (dolist (file srcs)
    (let ((backup (make-temp-file (file-name-base file) nil ".backup.org")))
      (unwind-protect
          ;; Prevent slow hooks from interfering
          (let (org-mode-hook org-confirm-babel-evaluate)
            ;; We do the ol' switcheroo because `org-babel-tangle' writes
            ;; changes to the current file, which would be imposing on the user.
            (copy-file file backup t)
            (with-current-buffer (find-file-noselect file)
              ;; Tangling doesn't expand #+INCLUDE directives, so we do it
              ;; ourselves, since includes are so useful for literate configs!
              (org-export-expand-include-keyword)
              (org-babel-tangle nil nil lang)))
        (ignore-errors (copy-file backup file t))
        (ignore-errors (delete-file backup)))))
  (kill-emacs 0))
