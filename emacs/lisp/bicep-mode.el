

;; Source: https://github.com/christiaan-janssen/bicep-mode/blob/master/bicep-mode.el

(defvar bicep-mode-hook nil)

(defvar bicep-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Bicep major mode")

(add-to-list 'auto-mode-alist '("\\.bicep\\'" . bicep-mode))


(defconst bicep-font-lock-keywords-1
  (let* ((x-keywords '("var" "resource" "params" "param" "target"))
         (x-types '("string" "integer" "object"))
         (x-constants '("true" "false"))
         (x-buildins '("module" "targetScope" "output" "existing" "for" "in"))
         (x-types-regexp (regexp-opt x-types 'words))
         (x-keywords-regexp (regexp-opt x-keywords 'words))
         (x-constants-regexp (regexp-opt x-constants 'words))
         (x-buildins-regexp (regexp-opt x-buildins 'words)))
    `((,x-types-regexp . 'font-lock-type-face)
      (,x-constants-regexp . 'font-lock-constant-face)
      (,x-buildins-regexp . 'font-lock-builtin-face)
      (,x-keywords-regexp . 'font-lock-keyword-face))))

(defvar bicep-mode-syntax-table nil)

(setq bicep-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?/ "<12" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        (modify-syntax-entry ?' "\"" synTable)
        (modify-syntax-entry ?\" "\"" synTable)
        (modify-syntax-entry ?{ "(}" synTable)
        (modify-syntax-entry ?} "){" synTable)
        synTable))

(define-derived-mode bicep-mode prog-mode "bicep mode"
  "Major mode for editing Bicep Language"
  (setq font-lock-defaults '((bicep-font-lock-keywords-1)))
  (set-syntax-table bicep-mode-syntax-table))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(bicep-mode . "bicep"))
  (lsp-register-client
   (make-lsp-client :new-connection(lsp-stdio-connection '("dotnet" "/usr/local/bin/bicep-langserver/Bicep.LangServer.dll"))
                    :activation-fn (lsp-activate-on "bicep")
                    :server-id 'bicep)))

(provide 'bicep-mode)
