;;; ../.myconf/emacs/doom/themes/one-dark-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Source: https://github.com/atom/one-dark-ui
;;
;;; Commentary:
;;
;; This themepack's flagship theme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup one-dark-theme nil
  "Options for the `one-dark' theme."
  :group 'doom-themes)

(defcustom one-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'one-dark-theme
  :type 'boolean)

(defcustom one-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'one-dark-theme
  :type 'boolean)

(defcustom one-dark-comment-bg one-dark-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'one-dark-theme
  :type 'boolean)

(defcustom one-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'one-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme one-dark
                "A dark theme inspired by Atom One Dark. (Custom extended)"

                ;; name        default   256           16
                ((bg         '("#282c34" "black"       "black"  ))
                 (fg         '("#bbc2cf" "#bfbfbf"     "brightwhite"  ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#21242b" "black"       "black"        ))
                 (fg-alt     '("#5B6268" "#2d2d2d"     "white"        ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#1B2229" "black"       "black"        ))
                 (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
                 (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
                 (base3      '("#23272e" "#262626"     "brightblack"  ))
                 (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
                 (base5      '("#5B6268" "#525252"     "brightblack"  ))
                 (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
                 (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
                 (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))


                 (grey          base4)
                 (red           '("#ff6c6b" "#ff6655" "red"          ))
                 (dark-red      '("#be5046" "#be5046" "red"          ))
                 (coral         '("#e06c75" "#ee6676" "brightred"    ))
                 (orange        '("#da8548" "#dd8844" "brightred"    ))
                 (light-green   '("#afc3a1" "#afc3a1" "brightgreen"  ))
                 (green         '("#98be65" "#99bb66" "green"        ))
                 (dim-green     '("#626e59" "#626e59" "brightgreen"  ))
                 (teal          '("#4db5bd" "#44b9b1" "brightgreen"  ))
                 (avocado       '("#aebe65" "#aebe65" "green"        ))
                 ;; (yellow        '("#ECBE7B" "#ECBE7B" "brightyellow" ))
                 (yellow        '("#e5c07b" "#e5c07b" "brightyellow" ))
                 (dark-yellow   '("#d19a66" "#d19a66" "yellow"       ))
                 (brown         '("#694f35" "#694f35" "yellow"       ))
                 ;; (blue          '("#51afef" "#51afef" "brightblue"   ))
                 (blue          '("#61afef" "#61afef" "blue"         ))
                 (dark-blue     '("#2257A0" "#2257A0" "blue"         ))
                 (magenta       '("#c678dd" "#c678dd" "brightmagenta"))
                 (violet        '("#a9a1e1" "#a9a1e1" "magenta"      ))
                 (pink          '("#c44482" "#c44482" "brightmagenta"))
                 (cyan          '("#46D9FF" "#46D9FF" "brightcyan"   ))
                 (dark-cyan     '("#5699AF" "#5699AF" "cyan"         ))
                 (fountain-blue '("#56b6c2" "#56b6c2" "cyan"         ))

                 ;; Custom extra colors

                 ;; These are the "universal syntax classes" that doom-themes establishes.
                 ;; These *must* be included in every doom themes, or your theme will throw an
                 ;; error, as they are used in the base theme defined in doom-themes-base.
                 (highlight      blue)
                 (vertical-bar   (doom-darken base1 0.1))
                 (selection      dark-blue)
                 (builtin        magenta)
                 (comments       (if one-dark-brighter-comments dark-cyan base5))
                 (doc-comments   (doom-lighten (if one-dark-brighter-comments dark-cyan base5) 0.25))
                 (constants      violet)
                 (functions      magenta)
                 (keywords       blue)
                 (methods        cyan)
                 (operators      blue)
                 (type           yellow)
                 (strings        green)
                 (variables      (doom-lighten magenta 0.4))
                 (numbers        orange)
                 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; Added syntax colors
                 (constructors      coral)
                 (characters        teal)
                 (labels            light-green)
                 (type-variables    coral)
                 (type-applications brown)
                 (quasiquotes       pink)
                 (hole              dark-blue)

                 ;; These are extra color variables used only in this theme; i.e. they aren't
                 ;; mandatory for derived themes.
                 (modeline-fg              fg)
                 (modeline-fg-alt          base5)
                 (modeline-bg              (if one-dark-brighter-modeline
                                               (doom-darken blue 0.45)
                                             (doom-darken bg-alt 0.1)))
                 (modeline-bg-alt          (if one-dark-brighter-modeline
                                               (doom-darken blue 0.475)
                                             `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
                 (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
                 (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

                 (-modeline-pad
                  (when one-dark-padded-modeline
                    (if (integerp one-dark-padded-modeline) one-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
                (((line-number &override) :foreground base4)
                 ((line-number-current-line &override) :foreground fg)
                 ((font-lock-comment-face &override)
                  :background (if one-dark-comment-bg (doom-lighten bg 0.05) 'unspecified))
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if one-dark-brighter-modeline base8 highlight))
   ;;;; tree-sitter
                 (tree-sitter-hl-face:haddock.multiline
                  :inherit font-lock-doc-face)
                 (tree-sitter-hl-face:haskell.label
                  :foreground labels)
                 (tree-sitter-hl-face:haskell.qq.punctuation
                  :foreground quasiquotes)
                 (tree-sitter-hl-face:haskell.qq.quoter
                  :foreground quasiquotes)
                 (tree-sitter-hl-face:haskell.qq.body
                  :background base3
                  :extend t)
                 (tree-sitter-hl-face:haskell.type.application.operator
                  :foreground type-applications)
                 (tree-sitter-hl-face:type.argument
                  :foreground type-variables)
                 (tree-sitter-hl-face:character
                  :foreground characters)
                 (tree-sitter-hl-face:constructor
                  :inherit font-lock-type-face
                  :weight 'medium
                  :foreground constructors)
                 (tree-sitter-hl-face:pragma
                  :inherit font-lock-preprocessor-face)
   ;;;; haskell-mode
                 (haskell-keyword-face :inherit font-lock-keyword-face)
                 (haskell-type-face :inherit font-lock-type-face)
                 (+haskell-constructor :inherit font-lock-type-face
                                       :weight 'medium
                                       :foreground constructors)
                 (+haskell-type-application-operator :inherit font-lock-type-face
                                                     :weight 'bold
                                                     :foreground brown)
                 (haskell-constructor-face :inherit font-lock-type-face
                                           :weight 'medium
                                           :foreground constructors)
                 (haskell-definition-face :inherit font-lock-function-name-face)
                 (haskell-operator-face :inherit font-lock-variable-name-face)
                 (haskell-pragma-face :inherit font-lock-preprocessor-face)
                 (haskell-liquid-haskell-annotation-face :inherit font-lock-preprocessor-face)
                 (haskell-quasi-quote-face :inherit font-lock-string-face
                                           :background base3
                                           :extend t)
                 (haskell-character-face :inherit font-lock-string-face
                                         :foreground characters)
   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; doom-modeline
                 (doom-modeline-bar :background (if one-dark-brighter-modeline modeline-bg highlight))
                 (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
                 (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
                 (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
                 (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
                 (font-latex-math-face :foreground green)
   ;;;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground red)
                 ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
                 (rjsx-tag :foreground red)
                 (rjsx-attr :foreground orange)
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
                ())

;;; one-dark-theme.el ends here
