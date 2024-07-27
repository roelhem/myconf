;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal Information


;; [[file:../../config.org::*Personal Information][Personal Information:1]]
(setq user-full-name "Roel Hemerik"
      user-mail-address "roel@shared.nl")
;; Personal Information:1 ends here

;; Garbage Collection


;; [[file:../../config.org::*Garbage Collection][Garbage Collection:1]]
(after! gcmh
  (setq gcmh-high-cons-threshold (* 64 1024 1024)))
;; Garbage Collection:1 ends here

;; LSP


;; [[file:../../config.org::*LSP][LSP:1]]
(setq read-process-output-max  (* 1024 1024)
      lsp-file-watch-threshold 2000)
;; LSP:1 ends here

;; Configuration Variables

;; Here, define the configuration variables for the function we define later on.


;; [[file:../../config.org::*Configuration Variables][Configuration Variables:1]]
(defconst myconf-config-org-file "~/.myconf/config.org"
  "The location of the config literal file.")

(defvar myconf-open-config-in-other-frame nil
  "Whether to open the config literal file in a new frame")

(defvar myconf-config-frame-name "Settings"
  "The name of the frame in which the settings should be opened.
Only has an effect of ~myconf-open-config-in-other-frame~ is non-nil")

(defvar myconf-config-workspace-name "*settings*"
  "The name of the workspace in which the settings will be opened.")
;; Configuration Variables:1 ends here

;; Open Settings

;; These functions allow me to quickly open this configuration from anywhere.


;; [[file:../../config.org::*Open Settings][Open Settings:1]]
(defun myconf-open-config-org ()
  "Open the myconf literal file."
  (interactive)
  (if myconf-open-config-in-other-frame
      (let ((frame (find-file-other-frame myconf-config-org-file)))
        (modify-frame-parameters nil `((fullscreen . fullscreen)
                                       (name . "Settings"))))
      (+workspace-switch myconf-config-workspace-name t)
      (find-file myconf-config-org-file)))
;; Open Settings:1 ends here



;; Bind this to the workspace prefix.


;; [[file:../../config.org::*Open Settings][Open Settings:2]]
(map! :leader
      :desc "Settings Workspace" "TAB ," #'myconf-open-config-org)
;; Open Settings:2 ends here



;; On MacOS, it should open the configuration just like any other application.


;; [[file:../../config.org::*Open Settings][Open Settings:3]]
(map! "s-," #'myconf-open-config-org)
;; Open Settings:3 ends here

;; Uuid

;; Inspired by:


;; [[file:../../config.org::*Uuid][Uuid:1]]
(defun xah-insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://xahlee.info/emacs/emacs/elisp_generate_uuid.html'
Version: 2020-06-04 2023-05-13"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((eq system-type 'darwin) ; Mac
    (shell-command "uuidgen" t))
   ((eq system-type 'gnu/linux)
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32)))))))
;; Uuid:1 ends here



;; But I need it also in lowercase.


;; [[file:../../config.org::*Uuid][Uuid:2]]
(defun roelhem/random-uuid ()
  (substring (shell-command-to-string "uuidgen") 0 -1))

(defun roelhem/random-lowercase-uuid ()
  (downcase (roelhem/random-uuid)))

(defun roelhem/insert-random-uuid-lowercase ()
  "Insert a lowercase UUID"
  (interactive)
  (insert (roelhem/random-lowercase-uuid)))
;; Uuid:2 ends here

;; Get JWT Body


;; [[file:../../config.org::*Get JWT Body][Get JWT Body:1]]
(defun jwt-parse-string (value &rest args)
  (let* ((parts (string-split value "\\."))
         (nth-decoded (lambda (n)
                        (apply 'json-parse-string
                         (base64-decode-string (nth n parts) t)
                         :object-type 'plist
                         :array-type 'list
                         args))))
    (append (mapcar nth-decoded '(0 1)) (nth 2 parts))))
;; Get JWT Body:1 ends here

;; Login


;; [[file:../../config.org::*Login][Login:1]]
(defun az-login ()
  (shell-command "az login --allow-no-subscriptions"))
;; Login:1 ends here

;; Getting Microsoft Graph Access Token


;; [[file:../../config.org::*Getting Microsoft Graph Access Token][Getting Microsoft Graph Access Token:1]]
(defun ms-graph--get-access-token (&rest scopes)
  "Returns a new ms-graph access token."
  (let ((command "az account get-access-token --resource-type ms-graph --query accessToken --output tsv ")
        (scope-str (string-join (cons "--scope" scopes) " ")))
    (substring (shell-command-to-string (concat command scope-str)) 0 -1)))
;; Getting Microsoft Graph Access Token:1 ends here

;; Setup


;; [[file:../../config.org::*Setup][Setup:1]]
(use-package! swagg
  :commands (swagg-request
             swagg-display-headers
             swagg-request-with-rest-block
             swagg-invalidate-cache))
;; Setup:1 ends here

;; APIs


;; [[file:../../config.org::*APIs][APIs:1]]
(setq
 swagg-definitions
 '((:name "GitHub"
    :json "https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json"
    :base "https://api.github.com")
   (:name "Azure AppConfiguration [stable v1.0]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/appconfiguration/data-plane/Microsoft.AppConfiguration/stable/1.0/appconfiguration.json")
   (:name "Azure WebPubSub [stable 2023-07-01]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/webpubsub/data-plane/WebPubSub/stable/2023-07-01/webpubsub.json")
   (:name "Azure KeyVault BackupRestore [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/backuprestore.json")
   (:name "Azure KeyVault Certificates [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/certificates.json")
   (:name "Azure KeyVault Common [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/common.json")
   (:name "Azure KeyVault Keys [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/keys.json")
   (:name "Azure KeyVault RBAC [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/rbac.json")
   (:name "Azure KeyVault Secrets [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/secrets.json")
   (:name "Azure KeyVault SecurityDomain [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/securitydomain.json")
   (:name "Azure KeyVault Settings [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/settings.json")
   (:name "Azure KeyVault Storage [stable v7.4]"
    :json "https://raw.githubusercontent.com/Azure/azure-rest-api-specs/main/specification/keyvault/data-plane/Microsoft.KeyVault/stable/7.4/storage.json")))
;; APIs:1 ends here

;; Htmlize


;; [[file:../../config.org::*Htmlize][Htmlize:1]]
(setq htmlize-html-major-mode 'web-mode
      htmlize-css-name-prefix "emacs-")
;; Htmlize:1 ends here

;; NX


;; [[file:../../config.org::*NX][NX:1]]
(load! "~/.myconf/emacs/lisp/nx-mode.el")

(map! :leader
      "p n f" #'nx-project-find-file
      "p n R" #'nx-project-run-target)
;; NX:1 ends here

;; Theme

;; This sets the default theme for emacs.


;; [[file:../../config.org::*Theme][Theme:1]]
(setq doom-theme 'one-dark)
;; Theme:1 ends here

;; One Dark

;; These are the colors of the original OneDark theme from Atom which I am used to.


;; [[file:../../config.org::*One Dark][One Dark:1]]
(defconst one-dark-colors
  '(
    (coral . "#e06c75")
    (vivid-coral . "#ef596f")
    (dark . "#5c6370")
    (deep . "#23272e")
    (invalid . "#ffffff")
    (light-dark . "#7f848e")
    (light-white . "#abb2bf")
    (malibu . "#61afef")
    (deep-red . "#be5046")
    (black . "#282c34")
    (white . "#abb2bf")
    (light-green . "#afc3a1")
    (green . "#98c379")
    (dim-green . "#626e59")
    (vivid-green . "#89ca78")
    (error-red . "#f44747")
    (light-red . "#e06c75")
    (dark-red . "#be5046")
    (chalky . "#e5c07b")
    (light-yellow . "#e5c07b")
    (wiskey . "#d19a66")
    (dark-yellow . "#d19a66")
    (vivid-fountain-blue . "#2bbac5")
    (fountain-blue . "#56b6c2")
    (blue . "#61afef")
    (purple . "#c678dd")
    (magenta . "#c678dd")
    (vivid-purple . "#d55fde")
    (pink . "#c44482")
    (cyan . "#56b6c2")
    (gutter-gray . "#4b5263")
    (comment-gray . "#5c6370")))
;; One Dark:1 ends here



;; The following function makes it easier to access these colors:


;; [[file:../../config.org::*One Dark][One Dark:2]]
(defun one-dark-color (name)
  "Selects one of the original one-dark colors with name NAME."
  (alist-get name one-dark-colors))
;; One Dark:2 ends here

;; Fonts

;; Fonts can be set using the following variables. /Source: The default emacs =config.el= file./

;; | variable                   | description                                                        |
;; |----------------------------+--------------------------------------------------------------------|
;; | ~doom-font~                | The primary font to use.                                           |
;; | ~doom-variable-pitch-font~ | a non-monospace font (where applicable)                            |
;; | ~doom-big-font~            | Used for ~doom-big-font-mode~ (during presentations or streaming). |
;; | ~doom-unicode-font~        | To show unicode glyphs                                             |
;; | ~doom-serif-font~          | For the ~fixed-pitch-serif~ face.                                  |



;; [[file:../../config.org::*Fonts][Fonts:1]]
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 15))
;; Fonts:1 ends here



;; Using ~s-=~ and ~s--~, changes the font size (default from /doom-emacs/). Lets make the step
;; as small as possible.


;; [[file:../../config.org::*Fonts][Fonts:2]]
(setq doom-font-increment 1)
;; Fonts:2 ends here

;; TreeSitter Highlighting

;; First some helper functions to make it easier to define extra tree-sitter faces.


;; [[file:../../config.org::*TreeSitter Highlighting][TreeSitter Highlighting:1]]
(defun tshelper--get-captures (queries)
  "Returns a list of all unique capture symbols in QUERIES."
  (-distinct
   (cl-loop for query being the elements of queries
            append (--filter (and (symbolp it) (string-prefix-p "@" (symbol-name it)))
                             (-flatten query)))))

(defun tshelper--declare-capture-faces (queries &optional fmt)
  "Declares tree-sitter-hl-face faces for each symbol in ITEMS.

Optinally use FMT to specify the format of the face symbol names."
  (let ((fmt (or fmt "tree-sitter-hl-face:%s"))
        (items (tshelper--get-captures queries)))
   (cl-loop for item in items
            collect (let* ((name (symbol-name item))
                           (symb (intern (format fmt (string-remove-prefix "@" name)))))
                     (custom-declare-face symb nil
                      (format "Face for capture %s" name))))))

(defun tshelper-add-patterns (lang aftr queries)
  "Adds tree sitter highlight patterns defined by QUERIES to language LANG."
  (tree-sitter-hl-add-patterns lang queries)
  (with-eval-after-load aftr
   (tshelper--declare-capture-faces queries)))
;; TreeSitter Highlighting:1 ends here



;; Then add some faces that are missing anyway


;; [[file:../../config.org::*TreeSitter Highlighting][TreeSitter Highlighting:2]]
(defface tree-sitter-hl-face:character nil nil)
;; TreeSitter Highlighting:2 ends here

;; Glyphs

;; The characters/strings used to indicate things in emacs.


;; [[file:../../config.org::*Glyphs][Glyphs:1]]
(setq truncate-string-ellipsis "…")
;; Glyphs:1 ends here

;; Date/Time


;; [[file:../../config.org::*Date/Time][Date/Time:1]]
(display-time-mode 1) ; Enables the display-time minor-mode.
;; Date/Time:1 ends here

;; Enabled widgets


;; [[file:../../config.org::*Enabled widgets][Enabled widgets:1]]
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu))
;; Enabled widgets:1 ends here

;; Source list


;; [[file:../../config.org::*Source list][Source list:1]]
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/alemuller/tree-sitter-make")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (php "https://github.com/tree-sitter/tree-sitter-php" "master")
        (bicep "https://github.com/tree-sitter-grammars/tree-sitter-bicep")
        (unison "https://github.com/fmguerreiro/tree-sitter-unison-kylegoetz" "build/include-parser-in-src-control")))
;; Source list:1 ends here

;; Bulk install


;; [[file:../../config.org::*Bulk install][Bulk install:1]]
(defun +treesit-install-all-language-grammars ()
  "Installs all tree sitter language grammars defined in ~treesit-language-source-alist~."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
;; Bulk install:1 ends here

;; Line numbers

;; This determines the style of line numbers in the editor.


;; [[file:../../config.org::*Line numbers][Line numbers:1]]
(setq display-line-numbers-type 'relative)
;; Line numbers:1 ends here

;; Behaviour


;; [[file:../../config.org::*Behaviour][Behaviour:1]]
(setq undo-limit (* 64 1024 1024)
      auto-save-default t)
;; Behaviour:1 ends here

;; Evil


;; [[file:../../config.org::*Evil][Evil:1]]
(setq evil-kill-on-visual-paste nil
      evil-respect-visual-line-mode t
      evil-ex-substitute-global t)
;; Evil:1 ends here

;; Insert (~SPC g~)


;; [[file:../../config.org::*Insert (~SPC g~)][Insert (~SPC g~):1]]
(map! :leader
      :prefix "i"
      :desc "Uppercase UUID" "G" #'xah-insert-random-uuid
      :desc "Lowercase UUID" "g" #'roelhem/insert-random-uuid-lowercase)
;; Insert (~SPC g~):1 ends here

;; Flycheck


;; [[file:../../config.org::*Flycheck][Flycheck:1]]
(after! flycheck
  (map! :leader
        :prefix ("e" . "explain")
        :desc "Error at point" "e" #'flycheck-explain-error-at-point
        :desc "Error list" "E" #'list-flycheck-errors))
;; Flycheck:1 ends here

;; LSP


;; [[file:../../config.org::*LSP][LSP:1]]
(after! lsp
  (map! :leader
        :prefix "e"
        :desc "Thing at point" "x" #'lsp-describe-thing-at-point
        :desc "LSP session" "L" #'lsp-describe-session))
;; LSP:1 ends here

;; TreeSitter Debug

;; Explain the syntax as seen by ~tree-sitter~.


;; [[file:../../config.org::*TreeSitter Debug][TreeSitter Debug:1]]
(map! :leader
      :prefix "t"
      :desc "TreeSitter Debug Mode" "t" #'tree-sitter-debug-mode)
;; TreeSitter Debug:1 ends here

;; TreeSitter Query


;; [[file:../../config.org::*TreeSitter Query][TreeSitter Query:1]]
(map! :leader
      :prefix "c"
      "q" #'tree-sitter-query-builder)
;; TreeSitter Query:1 ends here

;; Frames (~SPC F~)

;; Add a frames section to the leader.


;; [[file:../../config.org::*Frames (~SPC F~)][Frames (~SPC F~):1]]
(map! :leader
      :prefix ("F" . "frame")
      :desc "Clear frame" "c" #'doom/delete-frame-with-prompt
      :desc "Undelete frame" "u" #'undelete-frame)
;; Frames (~SPC F~):1 ends here

;; Special Files (~SPC o ,~)


;; [[file:../../config.org::*Special Files (~SPC o ,~)][Special Files (~SPC o ,~):1]]
(defconst rh/special-files/hosts "/etc/hosts"
  "Location of the hosts file")

(defun rh/special-files-open/hosts ()
  "Opens the hosts file"
  (interactive)
  (doom/sudo-find-file rh/special-files/hosts))

(map! :leader
      :prefix ("o ," . "special files")
      :desc rh/special-files/hosts "h" #'rh/special-files-open/hosts)
;; Special Files (~SPC o ,~):1 ends here

;; Page scrolling

;; I like ~h,j,k,l~! also ~s-h,s-j,s-k,s-l~ seem to be unused, so lets bind them so that they handle scrolling large buffers!


;; [[file:../../config.org::*Page scrolling][Page scrolling:1]]
(map! :n "s-h" #'evil-scroll-left
      :n "s-j" #'evil-scroll-down
      :n "s-k" #'evil-scroll-up
      :n "s-l" #'evil-scroll-left)
;; Page scrolling:1 ends here

;; Behaviour


;; [[file:../../config.org::*Behaviour][Behaviour:1]]
(setq which-key-idle-delay 0.5
      which-key-allow-multiple-replacements t
      which-key-show-operator-state-map t
      which-key-use-C-h-commands nil
      which-key-show-remaining-keys t)
;; Behaviour:1 ends here

;; Key replacements

;; Make the hints from =which-key= more readable by replacing common prefixes by symbols.


;; [[file:../../config.org::*Key replacements][Key replacements:1]]
(after! which-key
  (pushnew! which-key-replacement-alist
            '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1")))
  (which-key-add-key-based-replacements
    "g z" "multiple-cursors"))
;; Key replacements:1 ends here

;; Restart LSP


;; [[file:../../config.org::*Restart LSP][Restart LSP:1]]
(defun +lsp-restart ()
  "Restarts the LSP workspace"
  (interactive)
  (lsp-restart-workspace))

(map! :after lsp-mode
      :leader
      "h r l" #'+lsp-restart)
;; Restart LSP:1 ends here

;; Format buffer


;; [[file:../../config.org::*Format buffer][Format buffer:1]]
(map! :leader
      "b f" #'+format/buffer)
;; Format buffer:1 ends here

;; MacOS Fullscreen

;; By default, emacs uses a full-screen mode that works different from how other application-windows will be put in full-screen. It was set like this as most people didn't like the standard window-manager of MacOS.

;; Problem is, I do like it and I do not want to to deal with strange full-screen windows that block things behind them. Also, I like emacs to open in full-screen by default.


;; [[file:../../config.org::*MacOS Fullscreen][MacOS Fullscreen:1]]
(add-to-list 'initial-frame-alist '(fullscreen . fullscreen))
;; MacOS Fullscreen:1 ends here



;; And after I toggled full-screen, I want the initial window to be as large as possible.


;; [[file:../../config.org::*MacOS Fullscreen][MacOS Fullscreen:2]]
(add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
;; MacOS Fullscreen:2 ends here



;; To ensure that emacs uses the MacOS-native way of fullscreen on toggle, I overwrite the =toggle-frame-fullscreen= function.


;; [[file:../../config.org::*MacOS Fullscreen][MacOS Fullscreen:3]]
(defun toggle-frame-fullscreen (&optional frame)
  "Custom toggle-frame-fullscreen that works better with MacOS in my opinion"
  (interactive)
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (if (memq fullscreen '(fullscreen fullboth))
	(let ((fullscreen-restore (frame-parameter frame 'fullscreen-restore)))
	  (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	      (set-frame-parameter frame 'fullscreen fullscreen-restore)
	    (set-frame-parameter frame 'fullscreen nil)))
      (modify-frame-parameters
       frame `((fullscreen . fullscreen)
               (fullscreen-restore . ,fullscreen))))))
;; MacOS Fullscreen:3 ends here

;; Default directories


;; [[file:../../config.org::*Default directories][Default directories:1]]
(setq org-directory "~/org/")
;; Default directories:1 ends here

;; Project files


;; [[file:../../config.org::*Project files][Project files:1]]
(let ((ld 'lsp-file-watch-ignored-directories)
      (lf 'lsp-file-watch-ignored-files))
  (with-eval-after-load 'lsp-mode
    (add-to-list ld "[/\\\\]\\.nx\\'")
    (add-to-list ld "[/\\\\]vendor\\'")
    (add-to-list ld "[/\\\\]dist\\'")
    (add-to-list ld "[/\\\\]\\.postman\\'")
    (add-to-list ld "[/\\\\]\\.spago\\'")
    (add-to-list ld "[/\\\\]\\.phpunit.cache\\'")
    (add-to-list ld "[/\\\\]doomemacs"))
)
;; Project files:1 ends here

;; Json to TypeScript


;; [[file:../../config.org::*Json to TypeScript][Json to TypeScript:1]]
(defun rh/json-schema-to-typescript-buffer ()
  (interactive)
  (shell-command))
;; Json to TypeScript:1 ends here

;; Highlighting Patterns


;; [[file:../../config.org::*Highlighting Patterns][Highlighting Patterns:1]]
(tshelper-add-patterns 'haskell 'haskell-mode
   [((comment) @haddock.multiline
     (.match? @haddock.multiline "^\\{-[|^](?s:.*)-}"))

    ((comment) @comment)
    ((pragma) @pragma)

    ((label) @haskell.label)

    (quasiquote [(quasiquote_start) (quasiquote_bar) "|]"] @haskell.qq.punctuation)
    ((quoter) @haskell.qq.quoter)
    ((quasiquote_body) @haskell.qq.body)

    (exp_type_application \. ("@") @haskell.type.application.operator)

    (type_literal (con_list) @haskell.list.type)
    (exp_literal (con_list) @haskell.list.constructor)
    (pat_literal (con_list) @haskell.list.constructor)

    (type_literal (con_unit) @haskell.unit.type)
    (exp_literal (con_unit) @haskell.unit.constructor)
    (pat_literal (con_unit) @haskell.unit.constructor)

    (type_list ["[" "]"] @haskell.list.type.punctuation)

    (type_tuple [(comma) "(" ")"] @haskell.tuple.type.punctuation)

    (["{" "}" "(" ")" "[" "]"] @punctuation.bracket)

    ((type) @type)

    ((type_variable) @type.argument)

    ((strict_type) @type.strict)])
;; Highlighting Patterns:1 ends here

;; Highlighting Overrides

;; First, define a variable that holds the mappings of the capture names to the faces.


;; [[file:../../config.org::*Highlighting Overrides][Highlighting Overrides:1]]
(defvar +haskell-tree-sitter-hl-face-mapping nil
  "Holds the haskell specific mappings from capture names to faces.")
;; Highlighting Overrides:1 ends here



;; Also define a function that returns the associated type face symbol. Then, bind this to the ~haskell-mode~ local ~tree-sitter-hl-face-mapping-function~.


;; [[file:../../config.org::*Highlighting Overrides][Highlighting Overrides:2]]
(defun +haskell-tree-sitter-hl-face-custom-get (cap)
  "Returns the type face symbol associated with the capture group
CAP in the mapping ~+haskell-tree-sitter-hl-face-mapping~."
  (alist-get cap +haskell-tree-sitter-hl-face-mapping nil nil #'equal))

(add-hook! haskell-mode
           (add-function :before-until (local 'tree-sitter-hl-face-mapping-function) #'+haskell-tree-sitter-hl-face-custom-get))
;; Highlighting Overrides:2 ends here



;; We can then fill this mapping variable. This can be done repeatedly without re-evaluating the functions above.


;; [[file:../../config.org::*Highlighting Overrides][Highlighting Overrides:3]]
(setq +haskell-tree-sitter-hl-face-mapping
      '(("pragma" . +haskell-pragma)
        ("haddock.multiline" . font-lock-doc-face)
        ("type" . +haskell-type)
        ("haskell.type.application.operator" . +haskell-type-application-operator)
        ("haskell.list.type" . +haskell-type)
        ("haskell.list.constructor" . +haskell-constructor)
        ("constructor" . +haskell-constructor)
        ("haskell.unit.type" . +haskell-type)
        ("haskell.unit.constructor" . +haskell-constructor)
        ("haskell.list.type.punctuation" . +haskell-type-punctuation)
        ("haskell.tuple.type.punctuation" . +haskell-type-punctuation)
        ("haskell.label" . +haskell-label)
        ("haskell.qq.punctuation" . +haskell-qq-punctuation)
        ("haskell.qq.quoter" . +haskell-qq-quoter)
        ("haskell.qq.body" . +haskell-qq-body)
        ("type.strict" . +haskell-type-strict)))
;; Highlighting Overrides:3 ends here



;; Here, we define the extra faces we need.


;; [[file:../../config.org::*Highlighting Overrides][Highlighting Overrides:4]]
(custom-set-faces! '(+haskell-lambda-symbol :inherit font-lock-keyword-face))
;; Highlighting Overrides:4 ends here

;; [[file:../../config.org::*Highlighting Overrides][Highlighting Overrides:5]]
(defface +haskell-pragma nil nil)
(defface +haskell-lambda-symbol nil nil)
(defface +haskell-type-application-operator nil nil)
(defface +haskell-type-strict nil nil)
(defface +haskell-type nil nil)
(defface +haskell-type-constructor-punctuation nil nil)
(defface +haskell-type-punctuation nil nil)
(defface +haskell-constructor nil nil)
(defface +haskell-label nil nil)
(defface +haskell-qq-punctuation nil nil)
(defface +haskell-qq-quoter nil nil)
(defface +haskell-qq-body nil nil)

(custom-set-faces!
;;  `(+haskell-pragma :inherit haskell-pragma-face)
  `(+haskell-type-application-operator :weight bold)
  `(+haskell-type-strict :foreground ,(one-dark-color 'blue))
  `(+haskell-type :inherit font-lock-type-face)
  `(+haskell-type-punctuation :inherit font-lock-type-face)
;  `(+haskell-constructor :inherit font-lock-constructor-face
;                         :weight medium)
  `(+haskell-label :foreground ,(one-dark-color 'light-green))
  `(+haskell-qq-punctuation :foreground ,(one-dark-color 'pink))
  `(+haskell-qq-quoter :foreground ,(one-dark-color 'pink)
                       :weight normal)
  `(+haskell-qq-body :inherit org-block :foreground ,(one-dark-color 'green)))
;; Highlighting Overrides:5 ends here

;; Formatter


;; [[file:../../config.org::*Formatter][Formatter:1]]
(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "fourmolu"))
;; Formatter:1 ends here

;; Hoogle Config


;; [[file:../../config.org::*Hoogle Config][Hoogle Config:1]]
(custom-set-variables
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-interactive-types-for-show-ambiguous t))
;; Hoogle Config:1 ends here

;; Evil Bindings

;; Override some evil bindings so that insert mode will always insert at the prompt.


;; [[file:../../config.org::*Evil Bindings][Evil Bindings:1]]
(defun haskell-interactive--get-prompt-point ()
  "Gets the start of the the current prompt"
  (marker-position haskell-interactive-mode-prompt-start))

(defun haskell-interactive--get-end-of-line (&optional pt)
  "Gets the end of the line, bypassing line wraps.
If PT is specified, find it's end of the line instead of the end of the line at the current prompt"
  (save-excursion
    (when pt (goto-char pt))
    (end-of-line)
    (point)))

(defun +haskell-interactive-goto-current-prompt ()
  "Goes to the the cursor to the current prompt"
  (interactive)
    (when (not (haskell-interactive-at-prompt))
      (goto-char haskell-interactive-mode-prompt-start)))

(defun +haskell-interactive-append ()
  "Append text at the next prompt."
  (interactive)
  (if (haskell-interactive-at-prompt)
      (call-interactively #'evil-append)
      (goto-char haskell-interactive-mode-prompt-start)
      (call-interactively #'evil-append-line)))

(defun +haskell-interactive-append-line ()
  "Append to end of line of the next prompt."
  (interactive)
  (when (not (haskell-interactive-at-prompt))
    (goto-char haskell-interactive-mode-prompt-start))
  (call-interactively #'evil-append-line))

(defun +haskell-interactive-insert ()
  "Insert text at the next prompt."
  (interactive)
  (when (not (haskell-interactive-at-prompt))
    (goto-char haskell-interactive-mode-prompt-start))
  (call-interactively #'evil-insert))

(defun +haskell-interactive-insert-line ()
  "Insert at the start of the prompt."
  (interactive)
  (goto-char haskell-interactive-mode-prompt-start)
  (call-interactively #'evil-insert))

(evil-define-operator +haskell-interactive-delete (beg end type register yank-handler)
  "Modification of the evil-delete to work in haskell interactive buffer.
Delete text from BEG to END with TYPE
Save in REGISTER or the kill-ring with YANK_HANDLER"
  (interactive "<R><x><y>")
  (let* ((beg (max (or beg (point)) (haskell-interactive--get-prompt-point)))
         (end (min (or end beg) (haskell-interactive--get-end-of-line))))
    (evil-delete beg end type register)
    (when (eq type 'line)
      (haskell-interactive-mode-bol))))

(defun +haskell-interactive-open-below ()
  "Opens a new line. Opens a prompt of the cursor is not a te new line"
  (interactive)
  (cond ((<= (point) (haskell-interactive--get-prompt-point))
         (goto-char (point-max))
         (insert "\n")
         (haskell-interactive-mode-prompt)
         (call-interactively #'+haskell-interactive-insert))
        (t
         (call-interactively #'evil-open-below))))

(map! :after haskell-interactive-mode
      :map haskell-interactive-mode-map
      :n "][" #'haskell-interactive-mode-prompt-next
      :n "[[" #'haskell-interactive-mode-prompt-previous
      :n "a" #'+haskell-interactive-append
      :n "A" #'+haskell-interactive-append-line
      :n "d" #'+haskell-interactive-delete
      :n "i" #'+haskell-interactive-insert
      :n "I" #'+haskell-interactive-insert-line
      :n "o" #'+haskell-interactive-open-below
      :n "RET" #'haskell-interactive-mode-return)
;; Evil Bindings:1 ends here

;; Toggle print mode

;; This command toggles the mode in which the results of ghci will be displayed.


;; [[file:../../config.org::*Toggle print mode][Toggle print mode:1]]
(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode"
                              '("fundamental-mode"
                                "haskell-mode"
                                "ghc-core-mode")))))

(after! haskell-interactive-mode
  (setq haskell-interactive-mode-eval-mode 'haskell-mode))
;; Toggle print mode:1 ends here

;; Enable lookups

;; Enables lookups from a GHCI-window!


;; [[file:../../config.org::*Enable lookups][Enable lookups:1]]
(set-lookup-handlers! 'haskell-interactive-mode
  :definition #'haskell-mode-jump-to-def)
;; Enable lookups:1 ends here

;; Pop-ups

;; Pop-up of the repl itself:


;; [[file:../../config.org::*Pop-ups][Pop-ups:1]]
(after! haskell-session
 (defun +haskell-interactive-session-buffer? (name)
   "Checks if the provided name is an haskell interactive session"
   (let ((session-names (mapcar
                         (lambda (item) (format "*%s*" (alist-get 'name item)))
                         haskell-sessions)))
   (member name session-names)))
 (set-popup-rule! #'+haskell-interactive-session?
   :size 80
   :actions #'+display-buffer-in-side-window
   :side 'right
   :vslot -1
   :modeline nil))
;; Pop-ups:1 ends here



;; Pop-up that shows the errors:


;; [[file:../../config.org::*Pop-ups][Pop-ups:2]]
(set-popup-rule! "^\\*HS-Error\\*" :size 12 :quit t :vslot 0)
;; Pop-ups:2 ends here

;; Menu

;; Settings for the haskell menu.

;; Firstly, it should open in a popup!


;; [[file:../../config.org::*Menu][Menu:1]]
(after! haskell-mode
  (set-popup-rule!
    (lambda (arg) (string-equal arg haskell-menu-buffer-name))
    :size 6 :quit t :slot -1))
;; Menu:1 ends here



;; Then define the toggle function.


;; [[file:../../config.org::*Menu][Menu:2]]
(setq haskell-menu-buffer-name "*haskell-menu*")

(defun +haskell-menu/toggle ()
    "Toggles the Haskell sessions menu"
    (interactive)
    (or (get-buffer haskell-menu-buffer-name)
        (with-current-buffer (get-buffer-create haskell-menu-buffer-name) (haskell-menu-mode)))
    (if-let (win (get-buffer-window haskell-menu-buffer-name))
            (delete-window win)
            (pop-to-buffer haskell-menu-buffer-name)
            (haskell-menu-revert-function nil nil)))
;; Menu:2 ends here



;; Finally, we add a keybinding to toggle the haskell menu.


;; [[file:../../config.org::*Menu][Menu:3]]
(map! :leader :prefix "o" :n "h" #'+haskell-menu/toggle)
;; Menu:3 ends here

;; Via CLI

;; Defines how the hoogle command should be called from the CLI. Then also define an evil-command so that hoogle can be accessed via ~:hoogl ...~.


;; [[file:../../config.org::*Via CLI][Via CLI:1]]
(after! haskell-hoogle
  (setq haskell-hoogle-command "hoogle --count=40")

  (evil-define-command +evil:hoogle (&optional query)
    "Searches hoogle"
    (interactive "<a>")
    (haskell-hoogle query))

  (evil-ex-define-cmd "hoogl[e]" '+evil:hoogle))
;; Via CLI:1 ends here

;; Via Browser

;; First add hoogle to list of web lookups.


;; [[file:../../config.org::*Via Browser][Via Browser:1]]
(add-to-list '+lookup-provider-url-alist '("Hoogle" "https://hoogle.haskell.org/?hoogle=%s"))
;; Via Browser:1 ends here

;; Keybindings


;; [[file:../../config.org::*Keybindings][Keybindings:1]]
(map!
 :after haskell-hoogle
 :leader
 "s h" #'haskell-hoogle)
;; Keybindings:1 ends here

;; Fixes

;; Somehow, I needed to add this to make ~haskell-mode~ work...


;; [[file:../../config.org::*Fixes][Fixes:1]]
(setq flymake-allowed-file-name-masks nil)

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.hs\\'" haskell-flymake-init))
;; Fixes:1 ends here

;; For ~haskell-mode~

;; The mode for editing haskell files.


;; [[file:../../config.org::*For ~haskell-mode~][For ~haskell-mode~:1]]
(map! :after haskell-mode
      :map haskell-mode-map
      :localleader
      "r" #'haskell-process-load-file
      :desc "compile" "b" #'haskell-compile
      :desc "goto imports" "i" #'haskell-navigate-imports)
;; For ~haskell-mode~:1 ends here

;; For ~haskel-cabal-mode~


;; [[file:../../config.org::*For ~haskel-cabal-mode~][For ~haskel-cabal-mode~:1]]
(map! :after haskell-cabal
      :map haskell-cabal-mode-map
      :localleader
      :desc "compile" "b" #'haskell-compile)
;; For ~haskel-cabal-mode~:1 ends here

;; For ~haskell-interactive-mode~

;; The mode for running ~ghci~.


;; [[file:../../config.org::*For ~haskell-interactive-mode~][For ~haskell-interactive-mode~:1]]
(map! :after haskell-interactive-mode
      :map haskell-interactive-mode-map
      :localleader
      :desc "Toggle GHCI output" "t" #'haskell-interactive-toggle-print-mode)
;; For ~haskell-interactive-mode~:1 ends here

;; For ~haskell-error-mode~

;; The mode for errors that occur in ~ghci~.


;; [[file:../../config.org::*For ~haskell-error-mode~][For ~haskell-error-mode~:1]]
(map! :map haskell-error-mode-map
      :vinm "q" #'+popup/quit-window
      :vinm "<escape>" #'+popup/quit-window)
;; For ~haskell-error-mode~:1 ends here

;; LSP


;; [[file:../../config.org::*LSP][LSP:1]]
; (push '((unison-ts-mode) "127.0.0.1" 5757) eglot-server-programs)
;; LSP:1 ends here

;; No LSP format.

;; The lsp formatter is conflicting with prettier. Better to always use prettier if possible.


;; [[file:../../config.org::*No LSP format.][No LSP format.:1]]
(setq-hook! 'web-mode-hook +format-with-lsp nil)
;; No LSP format.:1 ends here

;; Configure Eglot


;; [[file:../../config.org::*Configure Eglot][Configure Eglot:1]]
(use-package! eglot-fsharp
  :defer t)
;; Configure Eglot:1 ends here

;; Bicep


;; [[file:../../config.org::*Bicep][Bicep:1]]
(add-to-list 'auto-mode-alist '("\\.bicep\\'" . bicep-ts-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(bicep-ts-mode . "bicep"))
  (lsp-register-client
   (make-lsp-client :new-connection(lsp-stdio-connection '("dotnet" "/usr/local/bin/bicep-langserver/Bicep.LangServer.dll"))
                    :activation-fn (lsp-activate-on "bicep")
                    :server-id 'bicep-langserver)))

(defun roelhem/bicep-ts-mode-tweaks ()
  (setq-local comment-start "// "))

(add-hook 'bicep-ts-mode-hook #'lsp!)
(add-hook 'bicep-ts-mode-hook #'roelhem/bicep-ts-mode-tweaks)
;; Bicep:1 ends here

;; Define Vue-mode for volar


;; [[file:../../config.org::*Define Vue-mode for volar][Define Vue-mode for volar:1]]
(define-derived-mode vue-mode web-mode "Vue"
  "A major mode derived from web-mode, for editing .vue files with volar language server.")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
;; Define Vue-mode for volar:1 ends here

;; Ensure Eglot uses Volar

;; The following function generates the volar configuration for eglot.


;; [[file:../../config.org::*Ensure Eglot uses Volar][Ensure Eglot uses Volar:1]]
(defun vue-eglot-init-options ()
             (let ((tsdk-path (expand-file-name
                               "lib"
                               (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))
                               )))
               `(:typescript (:tsdk ,tsdk-path
                              :languageFeatures (:completion
                                                 (:defaultTagNameCase "both"
                                                  :defaultAttrNameCase "kebabCase"
                                                  :getDocumentNameCasesRequest nil
                                                  :getDocumentSelectionRequest nil)
                                                 :diagnostics
                                                 (:getDocumentVersionRequest nil))
                              :documentFeatures (:documentFormatting
                                                 (:defaultPrintWidth 100
                                                  :getDocumentPrintWidthRequest nil)
                                                 :documentSymbol t
                                                 :documentColor t)))))
;; Ensure Eglot uses Volar:1 ends here



;; Then, we add it to the list of eglot language servers.


;; [[file:../../config.org::*Ensure Eglot uses Volar][Ensure Eglot uses Volar:2]]
(after! eglot
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
  (add-hook 'vue-mode-hook 'eglot-ensure))
;; Ensure Eglot uses Volar:2 ends here

;; Intelephense

;; The =lsp=-package fogot to implement some lsp-settings for the =intelephense=-server. Therefore, I'll add them here.


;; [[file:../../config.org::*Intelephense][Intelephense:1]]
(after! lsp
  (defcustom-lsp lsp-intelephense-document-root "apps/backend/public/index.php"
    "The directory of the entry point to the application (index.php)."
    :type 'string
    :group 'lsp-intelephense
    :lsp-path "intelephense.environment.documentRoot")
  (defcustom-lsp lsp-intelephense-include-paths []
    "The include paths"
    :type '(repeat string)
    :group 'lsp-intelephense
    :lsp-path "intelephense.environment.includePaths"))
;; Intelephense:1 ends here

;; Enable LSP-mode


;; [[file:../../config.org::*Enable LSP-mode][Enable LSP-mode:1]]
(add-hook 'csharp-tree-sitter-mode-hook #'lsp!)
;; Enable LSP-mode:1 ends here

;; Major-mode

;; A ~Brewfile~ is essentially just a stripped-down version of a ~ruby~ script. Therefore, we can use ~ruby-mode~ to define a new mode for Brewfiles.


;; [[file:../../config.org::*Major-mode][Major-mode:1]]
(define-derived-mode brewfile-mode ruby-mode "Brewfile")
;; Major-mode:1 ends here



;; For now, we will only activate this mode for files named =Brewfile= or files with the the =.Brewfile= extension.


;; [[file:../../config.org::*Major-mode][Major-mode:2]]
(add-to-list 'auto-mode-alist '("[/.]Brewfile\\'" . brewfile-mode))
;; Major-mode:2 ends here

;; Enable GraphQL in ~js~ and ~ts~.


;; [[file:../../config.org::*Enable GraphQL in ~js~ and ~ts~.][Enable GraphQL in ~js~ and ~ts~.:1]]
(after! mmm-mode
  (mmm-add-classes '((js-graphql
                      :submode graphql-mode
                      :face mmm-declaration-submode-face
                      :front "[^a-zA-Z]gql`"
                      :back "`")))
  (mmm-add-mode-ext-class 'typescript-ts-mode nil 'js-graphql)
  (setq mmm-global-mode 'maybe))
;; Enable GraphQL in ~js~ and ~ts~.:1 ends here

;; Feeds


;; [[file:../../config.org::*Feeds][Feeds:1]]
(setq elfeed-feeds
      '("https://opendata.cbs.nl/ODataCatalog/Tables"))
;; Feeds:1 ends here

;; Parameters


;; [[file:../../config.org::*Parameters][Parameters:1]]
(setq tidal-boot-script-path "~/workspace/tidal/BootTidal.hs")
;; Parameters:1 ends here

;; Setup


;; [[file:../../config.org::*Setup][Setup:1]]
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))
;; Setup:1 ends here

;; Keybindings


;; [[file:../../config.org::*Keybindings][Keybindings:1]]
(map! :after org-tree-slide
      :map org-tree-slide-mode-map
      "<f5>" 'org-tree-slide-move-previous-tree
      "<f6>" 'org-tree-slide-move-next-tree)
;; Keybindings:1 ends here

;; Keybindings

;; On the local leader:


;; [[file:../../config.org::*Keybindings][Keybindings:1]]
(map! :after org
      :map org-mode-map
      :localleader
      "H" #'org-insert-heading
      :desc "tangle" "RET" #'org-babel-tangle)
;; Keybindings:1 ends here

;; Yasnippets

;; Configures ~snippet-mode~ for ~yasnippets~.


;; [[file:../../config.org::*Yasnippets][Yasnippets:1]]
(map! :after yasnippet
      :map snippet-mode-map
      :localleader
      :desc "Load buffer" "b" #'yas-load-snippet-buffer
      :desc "Load buffer and close" "RET" #'yas-load-snippet-buffer-and-close
      :desc "Tryout snippet" "t" #'yas-tryout-snippet)
;; Yasnippets:1 ends here

;; Json null-characters

;; The emacs json-parser does not like null-characters. The following advices ensure that there are no null-characters in the input-strings of a json.


;; [[file:../../config.org::*Json null-characters][Json null-characters:1]]
(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))

(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
              (save-excursion
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
                  (apply oldfn args)))
;; Json null-characters:1 ends here
