;;; vue-ts-mode.el --- Major mode for editing Vue templates  -*- lexical-binding: t; -*-

;; More info:
;; README: https://github.com/8uff3r/vue-ts-mode
;; tree-sitter-vue: https://github.com/ikatyang/tree-sitter-vue
;; Vue: https://vuejs.org//

;;; Code:

(require 'treesit)
(require 'tstools)
(require 'typescript-ts-mode)
(require 'json-ts-mode)
(require 'css-mode)
(require 'rx)


(defgroup vue ()
  "Major mode for editing Vue templates."
  :group 'languages)

;; Indent Rules

(defcustom vue-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `vue-ts-mode'."
  :type 'integer
  :group 'vue
  :package-version '(vue-ts-mode . "1.0.0"))

(defvar vue-ts-mode--indent-rules
  `((vue
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol 0)
     ((parent-is "style_element") parent-bol 0)
     ((parent-is "template_element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol vue-ts-mode-indent-offset))
    (css . ,(append (alist-get 'css css--treesit-indent-rules)
                    '(((parent-is "stylesheet") parent-bol 0))))
    (typescript . ,(alist-get 'typescript (typescript-ts-mode--indent-rules 'typescript))))
  "Tree-sitter indentation rules for `vue-ts-mode'.")

;; font-lock rules
(defface vue-ts-mode-template-tag-bracket-face
  '((t :foreground "#86e1fc"))
  "Face for html tags angle brackets (<, > and />)."
  :group 'vue-ts-mode-faces)

(defface vue-ts-mode-html-tag-face
  '((t :inherit 'font-lock-keyword-face))
  "Face for standard tag names."
  :group 'vue-ts-mode-faces)

(defface vue-ts-mode-builtin-tag-face
  '((t :inherit 'font-lock-builtin-face))
  "Face for standard tag names."
  :group 'vue-ts-mode-faces)

(defface vue-ts-mode-directive-modifier-face
  '((t :slant italic :inherit 'font-lock-string-face))
  "Directive modifier face."
  :group 'vue-ts-mode-faces)

(defun vue-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
          settings))

(defvar vue-font-lock-settings
  (append
   (tstools-prefix-font-lock-features
    "typescript"
    (tstools-major-mode-font-lock-settings 'typescript-ts-mode))

   (tstools-prefix-font-lock-features
    "css"
    (tstools-major-mode-font-lock-settings 'css-ts-mode))

   (tstools-prefix-font-lock-features
    "json"
    (tstools-major-mode-font-lock-settings 'json-ts-mode))

   (treesit-font-lock-rules
    :language 'vue
    :override t
    :feature 'vue-comment
    '((comment) @font-lock-comment-face)

    :language 'vue
    :override t
    :feature 'vue-ref
    '((element (_ (attribute
                   (attribute_name)
                   @font-lock-type-face
                   (:equal @font-lock-type-face "ref")
                   (quoted_attribute_value
                    (attribute_value)
                    @font-lock-variable-name-face)))))

    :language 'vue
    :override t
    :feature 'vue-sp-dir
    `((_ (_ (directive_attribute
             (directive_name)
             @font-lock-keyword-face
             (:match ,(rx string-start
                          (or "v-text"
                              "v-html"
                              "v-show"
                              "v-if"
                              "v-else"
                              "v-else-if"
                              "v-for"
                              "v-on"
                              "v-bind"
                              "v-model"
                              "v-slot"
                              "v-pre"
                              "v-once"
                              "v-memo"
                              "v-cloak"
                              ":"
                              "@")
                          string-end)
                     @font-lock-keyword-face)))))

    :language 'vue
    :feature 'vue-attr
    '((attribute_name) @font-lock-type-face)

    :language 'vue
    :override t
    :feature 'vue-definition
    '((tag_name) @vue-ts-mode-builtin-tag-face)

    :language 'vue
    :feature 'vue-directive
    '((_ (_
          (directive_attribute
           (directive_name) @font-lock-keyword-face
           (directive_argument) @font-lock-property-use-face)))
      (_ (_
          (directive_attribute
           (directive_name) @font-lock-variable-name-face))))

    :language 'vue
    :feature 'vue-modifier
    '(((directive_modifiers) @vue-ts-mode-directive-modifier-face))

    :language 'vue
    :override t
    :feature 'vue-bracket
    '((["<" ">" "</" "/>"]) @vue-ts-mode-template-tag-bracket-face
      (["{{" "}}"]) @font-lock-misc-punctuation-face)

    :language 'vue
    :feature 'vue-string
    '((attribute (quoted_attribute_value) @font-lock-string-face))

    :language 'vue
    :override t
    :feature 'vue-element
    `((element (_ (tag_name) @vue-ts-mode-html-tag-face))
      (element (_ (tag_name) @font-lock-keyword-face
                  (:match ,(rx string-start
                               (or "slot"
                                   "Transition"
                                   "transition-group"
                                   "TransitionGroup"
                                   "keep-alive"
                                   "KeepAlive"
                                   "Teleport"
                                   "Suspense")
                               string-end) @font-lock-keyword-face))))

    :language 'vue
    :override t
    :feature 'vue-component
    `((element
       (_ (tag_name) @rh-font-lock-constructor-face
          (:match ,(rx string-start
                       (in (?A . ?Z))
                       (* (in (?A . ?Z) (?a . ?z) (?0 . ?9)))
                       string-end) @rh-font-lock-constructor-face)))))))

(defvar vue-ts-mode--range-settings
  (treesit-range-rules

   :embed 'typescript
   :host 'vue
   '((script_element (raw_text) @capture))

   ;; :embed 'typescript
   ;; :host 'vue
   ;; '((interpolation (raw_text) @capture))

   ;; :embed 'typescript
   ;; :host 'vue
   ;; '((directive_attribute
   ;;   (quoted_attribute_value (attribute_value) @capture)))

   :embed 'css
   :host 'vue
   '((style_element (raw_text) @capture))

   :embed 'json
   :host 'vue
   `((component
      (element (start_tag
                (tag_name) @_d
                (:equal "i18n" @_d))
               (text) @capture)))))

(defun vue-ts-mode--advice-for-treesit-buffer-root-node (&optional lang)
  "Return the current ranges for the LANG parser in the current buffer.

If LANG is omitted, return ranges for the first language in the parser list.

If `major-mode' is currently `vue-ts-mode', or if LANG is vue, this function
instead always returns t."
  (if (or (eq lang 'vue) (not (eq major-mode 'vue-ts-mode)))
      t
    (treesit-parser-included-ranges
     (treesit-parser-create

      (or lang (treesit-parser-language (car (treesit-parser-list))))))))

(defun vue-ts-mode--advice-for-treesit--merge-ranges (_ new-ranges _ _)
  "Return truthy if `major-mode' is `vue-ts-mode', and if NEW-RANGES is non-nil."
  (and (eq major-mode 'vue-ts-mode) new-ranges))

(defun vue-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

(defun vue-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range))
                             (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (or language-in-range 'vue)))
;;;###autoload
(define-derived-mode vue-ts-mode prog-mode "Vue-ts"
  "Major mode for editing Vue templates, powered by tree-sitter."
  :group 'vue
  ;; :syntax-table html-mode-syntax-table

  (unless (treesit-ready-p 'vue)
    (error "Tree-sitter grammar for Vue isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'typescript)
    (error "Tree-sitter grammar for Typescript/TYPESCRIPT isn't available"))

  (when (treesit-ready-p 'typescript)
    (treesit-parser-create 'vue)

    ;; Comments and text content
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment" "text")))

    ;; Indentation rules
    (setq-local treesit-simple-indent-rules vue-ts-mode--indent-rules
                css-indent-offset vue-ts-mode-indent-offset)

    ;; Font locking
    (setq-local treesit-font-lock-settings vue-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                (tstools--merge-font-lock-feature-lists
                 (list
                  ;;(tstools-prefix-font-lock-feature-list
                  ;; "typescript"
                  ;; (tstools-major-mode-font-lock-feature-list 'typescript-ts-mode))
                  ;; (tstools-prefix-font-lock-feature-list
                  ;;  "tsx"
                  ;;  (tstools-major-mode-font-lock-feature-list 'tsx-ts-mode))
                  ;; (tstools-prefix-font-lock-feature-list
                  ;;  "css"
                  ;;  (tstools-major-mode-font-lock-feature-list 'css-ts-mode))
                  '((typescript-comment
                     typescript-declaration
                     json-comment
                     json-constant
                     json-number
                     json-pair
                     json-string
                     vue-attr
                     vue-toplevel
                     vue-definition
                     vue-directive
                     vue-comment
                     vue-element
                     vue-component)
                    (typescript-keyword
                     typescript-string
                     typescript-escape-sequence
                     json-escape-sequence
                     vue-ref
                     vue-string
                     vue-directive)
                    (typescript-constant
                     typescript-expression
                     typescript-identifier
                     typescript-number
                     typescript-pattern
                     typescript-property
                     json-bracket
                     json-delimiter
                     json-error
                     vue-sp-dir
                     vue-bracket
                     vue-modifier
                     vue-block)
                    (typescript-constructor
                     typescript-type-import
                     typescript-property-member
                     typescript-template-string
                     typescript-constructor-bracket
                     typescript-method
                     typescript-label
                     typescript-operator
                     typescript-function
                     typescript-bracket
                     typescript-delimiter)))))

    ;; Embedded languages
    (setq-local treesit-range-settings vue-ts-mode--range-settings)
    (setq-local treesit-language-at-point-function
                #'vue-ts-mode--treesit-language-at-point)
    (treesit-major-mode-setup)))

(if (treesit-ready-p 'vue)
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode)))

(advice-add
 #'treesit-buffer-root-node
 :before-while
 #'vue-ts-mode--advice-for-treesit-buffer-root-node)

(advice-add
 #'treesit--merge-ranges
 :before-while
 #'vue-ts-mode--advice-for-treesit--merge-ranges)

(provide 'vue-ts-mode)
;;; vue-ts-mode.el ends here
