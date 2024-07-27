;;; $DOOMDIR/packages.el -*- lexical-binding: t; -*-

;; Enabling packages


;; [[file:../../config.org::*Enabling packages][Enabling packages:1]]
(package! homebrew-mode)

(package! company-nginx)
(package! nginx-mode)
(package! x509-mode)

(package! tidal)
(package! xkcd)

(package! poly-markdown)
(package! polymode)
(package! ob-fsharp)

(package! org-tree-slide)

(package! evil-cleverparens)

(package! eglot-fsharp)
(package! bicep-ts-mode)

(package! mmm-mode)

(package! swagg)

(package! kurecolor)

(package! unison-ts-mode
  :recipe (:type git
           :host github
           :repo "fmguerreiro/unison-ts-mode"
           :files ("*.el")))
;; Enabling packages:1 ends here

;; Disabling default packages


;; [[file:../../config.org::*Disabling default packages][Disabling default packages:1]]
(package! evil-escape :disable t)
;; Disabling default packages:1 ends here
