(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("52e195b678027dccce6f8dec161146718c42d3daad1b488119539f953c529b86" "2c2fab6eefc7a99c87ec8c8faabd55425a27a3b54fa5dcabd63c3ed96f757ad1" "f2760f0aa88c13c33a19b0347e43721c153af440aaff5bcf686cacda1115465a" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(elfeed-feeds '("https://opendata.cbs.nl/ODataCatalog/Tables") t)
 '(haskell-interactive-types-for-show-ambiguous t)
 '(haskell-process-suggest-hoogle-imports t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(org-plus-contrib))
 '(safe-local-variable-values
   '((lsp-csharp-solution-file . "/Users/roel/workspace/precon/PreconDotnetServices.sln")
     (lsp-csharp-solution-file . "~/workspace/precon/PreconDotnetServices.sln")
     (lsp-csharp-solution-file . ~/workspace/precon/PreconDotnetServices.sln)
     (php-project-use-projectile-to-detect-root . t)
     (lsp-intelephense-files-max-size . 8388608)
     (lsp-intelephense-stubs .
      ["bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "ds" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "msql" "msqli" "oci8" "odbc" "openssl" "Parle" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "redis" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend" "OPcache" "zip" "zlib"])
     (lsp-intelephense-rename-exclude .
      ["**/vendor/**"])
     (lsp-intelephense-format-enable . t)
     (lsp-intelephense-format-braces . "psr12")
     (lsp-intelephense-file-associations .
      ["*.php" "*.html"])
     (lsp-intelephense-php-version . "8.2")))
 '(warning-suppress-log-types '((lsp-mode) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+haskell-constructor ((t (:inherit font-lock-constructor-face :weight medium))) t)
 '(+haskell-label ((t (:foreground "#afc3a1"))) t)
 '(+haskell-lambda-symbol ((t (:inherit font-lock-keyword-face))) t)
 '(+haskell-pragma ((t (:inherit haskell-pragma-face))) t)
 '(+haskell-qq-body ((t (:inherit org-block :foreground "#98c379"))) t)
 '(+haskell-qq-punctuation ((t (:foreground "#c44482"))) t)
 '(+haskell-qq-quoter ((t (:foreground "#c44482" :weight normal))) t)
 '(+haskell-type ((t (:inherit font-lock-type-face))) t)
 '(+haskell-type-application-operator ((t (:foreground "orange4" :weight bold))) t)
 '(+haskell-type-constructor-punctuation ((t (:inherit font-lock-type-decorator-face))) t)
 '(+haskell-type-punctuation ((t (:inherit font-lock-type-face))) t)
 '(+haskell-type-strict ((t (:inherit font-lock-type-decorator-face :foreground "#61afef"))) t)
 '(font-lock-character-face ((t (:foreground "#56b6c2"))))
 '(font-lock-constructor-face ((t (:foreground "#e06c75"))))
 '(tree-sitter-hl-face:character ((t (:inherit font-lock-character-face))) t)
 '(tree-sitter-hl-face:constructor ((t (:inherit font-lock-constructor-face))))
 '(tree-sitter-hl-face:haddock.multiline ((t (:inherit font-lock-doc-face))) t)
 '(tree-sitter-hl-face:type.argument ((t (:inherit font-lock-type-argument-face))))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'customize-apropos-faces 'disabled nil)
