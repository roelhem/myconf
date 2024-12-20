{
  config,
  lib,
  ...
}:

let

  inherit (lib)
    mkOption
    mkIf
    types
    attrsets
    length
    attrNames
    elisp
    ;

  cfg = config.doom.init;

  mkInitOption =
    description: flags:
    let
      enable = mkOption {
        type = types.bool;
        default = false;
        description = description;
      };

      mkFlagOpt = flag: {
        name = flag;
        value = mkOption {
          type = types.bool;
          default = false;
          description = "+${flag}";
        };
      };

      flagOpts = builtins.listToAttrs (builtins.map mkFlagOpt flags);
    in
    { inherit enable; } // flagOpts;

  doomInitArgs =
    let
      categories = [
        "completion"
        "ui"
        "input"
        "editor"
        "emacs"
        "term"
        "checkers"
        "tools"
        "os"
        "lang"
        "email"
        "app"
        "config"
      ];

      toInitItem =
        name: args:
        if args.enable then
          (
            let
              flags = builtins.filter (x: x != null) (
                attrsets.mapAttrsToList (
                  flagName: enabled: if flagName == "enable" || !enabled then null else "+${flagName}"
                ) args
              );
            in
            (if length flags > 0 then "(${name} ${builtins.concatStringsSep " " flags})" else name)
          )
        else
          ";;${name} ;";

      toInitItems =
        catName:
        let
          catOpts = cfg."${catName}";

          items = builtins.filter (x: x != null) (attrsets.mapAttrsToList toInitItem catOpts);
        in
        ([ ":${catName}" ] ++ items ++ [ "" ]);
    in
    builtins.concatMap toInitItems categories;
in

{
  options.doom.init = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether the init.el file should be included in this doom config.";
    };

    setq = mkOption {
      type = types.attrsOf types.anything;
      default = { };
      description = "Attributes that will be converted to setq definitions at the very top of the config.";
    };

    extraConfig = mkOption {
      type = types.separatedString "\n\n";
      default = "";
      description = "Extra configuration that will be added before the (doom!) statement in the resulting init.el file.";
    };

    input = {
      bidi = mkInitOption "Enable (tefl ot) thgir etirw uoy gnipleh" [ ];
      chinese = mkInitOption "Enable chinese support" [
        "rime"
        "childframe"
      ];
      japanese = mkInitOption "Enable japanese support" [ ];
      layout = mkInitOption "Enable auie,ctsrnm is the superior home row" [
        "azerty"
        "bepo"
      ];
    };

    completion = {
      company = mkInitOption "Enable the ultimate code completion backend" [
        "childframe"
        "tng"
      ];
      corfu = mkInitOption "Enable complete with cap(f), cape and flying feather!" [
        "icons"
        "orderless"
        "dabbrev"
      ];
      helm = mkInitOption "Enable the *other* search enginge for love and life" [
        "childframe"
        "fuzzy"
        "icons"
      ];
      ido = mkInitOption "Enable the other *other* search engine..." [ ];
      ivy = mkInitOption "Enable a search engine for love and life" [
        "childframe"
        "fuzzy"
        "icons"
        "prescient"
      ];
      vertico = mkInitOption "Enable the search engine of the future" [
        "childframe"
        "icons"
      ];
    };
    ui = {
      deft = mkInitOption "Enable notation velocity for Emacs" [ ];
      doom = mkInitOption "Enable what makes DOOM look the way it does" [ ];
      doom-dashboard = mkInitOption "Enable a nifty splash scree for Emacs" [ ];
      doom-quit = mkInitOption "DOOM quit-message prompts when you quit Emacs" [ ];
      emoji = mkInitOption "Enable emoji" [
        "ascii"
        "github"
        "unicode"
      ];
      hl-todo = mkInitOption "Enable highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW" [ ];
      indent-guides = mkInitOption "Enable highlighted indent columns" [ ];
      ligatures = mkInitOption "Enable ligatures and symbols to make your code pretty again" [ "extra" ];
      minimap = mkInitOption "Enable show a map of the code on the side" [ ];
      modeline = mkInitOption "Enable blink cursor line after big motions" [ "light" ];
      nav-flash = mkInitOption "Enable blink cursor line after big motions" [ ];
      neotree = mkInitOption "Enable a project drawer, like NERDTree for vim" [ ];
      ophints = mkInitOption "Enable highlight the region an operation acts on" [ ];
      popup = mkInitOption "Enable tame sudden yet inevitable temporary windows" [
        "all"
        "defaults"
      ];
      tabs = mkInitOption "Enable a tab bar for Emacs" [ ];
      treemacs = mkInitOption "Enable a project drawer, like neotree but cooler" [ "lsp" ];
      unicode = mkInitOption "Enable extended unicode support for various languages" [ ];
      vc-gutter = mkInitOption "Enable vcs diff in the fringe" [
        "diff-hl"
        "pretty"
      ];
      vi-tilde-fringe = mkInitOption "Enable fringe tildes to makr beyond EOB" [ ];
      window-select = mkInitOption "Enable visually switch windows" [
        "numbers"
        "switch-window"
      ];
      workspaces = mkInitOption "Enable tab emulation, persistence & separate workspaces" [ ];
      zen = mkInitOption "Enable distraction-free coding or writing" [ ];
    };
    editor = {
      evil = mkInitOption "Enable come to the dark side, we have cookies" [ "everywhere" ];
      file-templates = mkInitOption "Enable auto-snippets for empty files" [ ];
      fold = mkInitOption "Enable (nigh) universal code folding" [ ];
      format = mkInitOption "Enable automated prettyness" [ "onsave" ];
      god = mkInitOption "Enable run Emacs commands without modifier keys" [ ];
      lispy = mkInitOption "Enable vim for lisp, for people who don't like vim" [ ];
      multiple-cursors = mkInitOption "Enable editing in may places at once" [ ];
      objed = mkInitOption "Enable text object editing for the innocent" [ "manual" ];
      parinfer = mkInitOption "Enable turn lisp into python, sort of" [ ];
      rotate-text = mkInitOption "Enable cycle region at point between text candidates" [ ];
      snippets = mkInitOption "Enable my elves. They type so I don't have to" [ ];
      word-wrap = mkInitOption "Enable soft wrapping with language-aware indent" [ ];
    };
    emacs = {
      dired = mkInitOption "Enable making dired pretty [functional]" [
        "icons"
        "ranger"
        "dirvish"
      ];
      electric = mkInitOption "Enable smarter, keyword-based electric-indent" [ ];
      ibuffer = mkInitOption "Enable interactive buffer management" [ "icons" ];
      undo = mkInitOption "Enable persistent, smarter undo for your inevitable mistakes" [ "tree" ];
      vc = mkInitOption "Enable version-control and Emacs, sitting in a tree" [ ];
    };
    term = {
      eshell = mkInitOption "Enable the elisp shell that works everywhere" [ ];
      shell = mkInitOption "Enable simple shell REPL for Emacs" [ ];
      term = mkInitOption "Enable basic terminal emulator for Emacs" [ ];
      vterm = mkInitOption "Enable the best terminal emulation in Emacs" [ ];
    };
    checkers = {
      syntax = mkInitOption "Enable tasing you for every semicolon you forget" [
        "childframe"
        "flymake"
      ];
      spell = mkInitOption "Enable tasing you for misspelling mispelling" [
        "aspell"
        "enchant"
        "everywhere"
        "flyspell"
        "hunspell"
      ];
      grammar = mkInitOption "Enable tasing grammar mistake every you make" [ ];
    };
    tools = {
      ansible = mkInitOption "" [ ];
      biblio = mkInitOption "Enable writes a PhD for you (citation needed)" [ ];
      collab = mkInitOption "Enable buffers with friends" [ "tunnel" ];
      debugger = mkInitOption "Enable FIXME stepping through code, to help you add bugs" [
        "lsp"
        "auth"
      ];
      direnv = mkInitOption "" [ ];
      docker = mkInitOption "" [ "lsp" ];
      editorconfig = mkInitOption "" [ ];
      ein = mkInitOption "Enable tame Jupyter notebooks with emacs" [ ];
      eval = mkInitOption "Enable run code, run (also, repls)" [ "overlay" ];
      lookup = mkInitOption "Enable navigate your code and its documentation" [
        "dictionary"
        "docset"
        "offline"
      ];
      lsp = mkInitOption "Enable M-x vscode" [
        "eglot"
        "peek"
      ];
      magit = mkInitOption "Enable a git porcelain for Emacs" [ "forge" ];
      make = mkInitOption "Enable run make tasks from Emacs" [ ];
      pass = mkInitOption "Enable password manager for nerds" [ "auth" ];
      pdf = mkInitOption "Enable pdf enhancements" [ ];
      prodigy = mkInitOption "FIXME managing external services & code builders" [ ];
      terraform = mkInitOption "Enable infrastructure as code" [ "lsp" ];
      taskrunner = mkInitOption "Enable taskrunner for all your projects" [ ];
      tmux = mkInitOption "Enable an API for interacting with tmux" [ ];
      tree-sitter = mkInitOption "Enable syntax and parsing, sitting in a tree..." [ ];
      upload = mkInitOption "Enable map local to remote projects via ssh/tfp" [ ];
    };
    os = {
      macos = mkInitOption "Enable improve compatibility with macOS" [ ];
      tty = mkInitOption "Enable improve the terminal Emacs experience" [ "osc" ];
    };
    lang = {
      agda = mkInitOption "Enable types of types of types of types..." [
        "local"
        "tree-sitter"
      ];
      beancount = mkInitOption "Enable mind the GAAP" [ "lsp" ];
      cc = mkInitOption "Enable C > C++ == 1" [
        "lsp"
        "tree-sitter"
      ];
      clojure = mkInitOption "Enable java with a lisp" [
        "lsp"
        "tree-sitter"
      ];
      common-lisp = mkInitOption "Enable if you've seen one lisp, you've seen them all" [ ];
      coq = mkInitOption "Enable proofs-as-programs" [ ];
      crystal = mkInitOption "Enable ruby at the speed of c" [ ];
      csharp = mkInitOption "Enable unity, .NET, and mono shenanigans" [
        "dotnet"
        "lsp"
        "tree-sitter"
        "unity"
      ];
      data = mkInitOption "Enable config/data formats" [ ];
      dart = mkInitOption "Enable paint ui and not much else" [
        "flutter"
        "lsp"
      ];
      dhall = mkInitOption "Enable the calculus of configuration" [ ];
      elixir = mkInitOption "Enable erlang done right" [
        "lsp"
        "tree-sitter"
      ];
      elm = mkInitOption "Enable care for a cup of TEA?" [
        "lsp"
        "tree-sitter"
      ];
      emacs-lisp = mkInitOption "Enable drown in parenthesis" [ ];
      erlang = mkInitOption "Enable an elegant language for a more civilized age" [
        "lsp"
        "tree-sitter"
      ];
      ess = mkInitOption "Enable emacs speaks statistics" [
        "stan"
        "tree-sitter"
      ];
      factor = mkInitOption "Enable factor" [ ];
      faust = mkInitOption "Enable dsp, but you get to keep your soul" [ ];
      fortran = mkInitOption "Enable in FORTRAN, GOD is REAL (unless declared INTEGER)" [
        "lsp"
        "intel"
      ];
      fsharp = mkInitOption "Enable ML stands for Microsoft's Language" [ "lsp" ];
      fstar = mkInitOption "Enable (dependent) types and (monadic) effects and Z3" [ ];
      gdscript = mkInitOption "Enable the language you waited for" [ "lsp" ];
      go = mkInitOption "Enable the hipster dialect" [
        "lsp"
        "tree-sitter"
      ];
      graphql = mkInitOption "Enable give queries a REST" [ "lsp" ];
      haskell = mkInitOption "Enable a language that's lazier than I am" [
        "lsp"
        "tree-sitter"
      ];
      hy = mkInitOption "Enable readability of scheme w/ speed of python" [ ];
      idris = mkInitOption "Enable a language you can depend on" [ ];
      json = mkInitOption "Enable at least it ain't XML" [
        "lsp"
        "tree-sitter"
      ];
      java = mkInitOption "Enable the poster child for carpal tunnel syndrome" [
        "lsp"
        "meghanada"
        "tree-sitter"
      ];
      javascript = mkInitOption "Enable all(hope(abandon(ye(who(enter(here))))))" [
        "lsp"
        "tree-sitter"
      ];
      julia = mkInitOption "Enable a better, faster MATLAB" [
        "lsp"
        "tree-sitter"
        "snail"
      ];
      kotlin = mkInitOption "Enable a better, slicker Java(Script)" [ "lsp" ];
      latex = mkInitOption "Enable writing papares in Emacs has never been so fun" [
        "cdlatex"
        "fold"
        "latexmk"
        "lsp"
      ];
      lean = mkInitOption "Enable for folks with too much to prove" [ ];
      ledger = mkInitOption "Enable be audit you can be" [ ];
      lua = mkInitOption "Enable lua" [
        "fennel"
        "lsp"
        "tree-sitter"
        "moonscript"
      ];
      markdown = mkInitOption "Enable writing docs for people to ignore" [ "grip" ];
      nim = mkInitOption "Enable python + lisp at the speed of c" [ ];
      nix = mkInitOption "Enable I hereby declare \"nix geht mehr!\"" [
        "tree-sitter"
        "lsp"
      ];
      ocaml = mkInitOption "Enable an objective camel" [
        "lsp"
        "tree-sitter"
      ];
      org = mkInitOption "Enable organize your plain life in plain text" [
        "brain"
        "contacts"
        "dragndrop"
        "crypt"
        "gnuplot"
        "hugo"
        "ipython"
        "journal"
        "jupyter"
        "noter"
        "pandoc"
        "passwords"
        "pomodoro"
        "present"
        "pretty"
        "roam"
        "roam2"
      ];
      php = mkInitOption "Eanble perl's insecure younger brother" [
        "hack"
        "lsp"
        "tree-sitter"
      ];
      plantuml = mkInitOption "Enable diagrams for confusing people more" [ ];
      graphviz = mkInitOption "Enable diagrams for confusing yourself even." [ ];
      purescript = mkInitOption "Enable javascript, but functional" [ "lsp" ];
      python = mkInitOption "Enable beautiful is better then ugly" [
        "conda"
        "cython"
        "lsp"
        "poetry"
        "pyenv"
        "pyright"
        "tree-sitter"
      ];
      qt = mkInitOption "Enable the `cutest` gui framework ever" [ ];
      racket = mkInitOption "Enable a DSL for DSLs" [
        "lsp"
        "xp"
      ];
      raku = mkInitOption "Enable the artist formerly known as perl6" [ ];
      rest = mkInitOption "Enable Emacs as a REST client" [ "jq" ];
      rst = mkInitOption "Enable ReST in peace" [ ];
      ruby = mkInitOption "Enable 1.step {|i| p \"Ruby is #{i.even? ? 'love' : 'life'}\"}" [
        "chruby"
        "lsp"
        "rails"
        "rbenv"
        "rmv"
        "tree-sitter"
      ];
      rust = mkInitOption "Enable Fe2O3.unwrap().unwrap().unwrap().unwrap()" [
        "lsp"
        "tree-sitter"
      ];
      scala = mkInitOption "Enable java, but good" [
        "lsp"
        "tree-sitter"
      ];
      scheme = mkInitOption "Enable a fully conniving family of lisps" [
        "chez"
        "chibi"
        "chicken"
        "gambit"
        "gauche"
        "guile"
        "kawa"
        "mit"
        "racket"
      ];
      sh = mkInitOption "Enable she sells {ba,z,fi}sh shells on the C xor" [
        "fish"
        "lsp"
        "powershell"
        "tree-sitter"
      ];
      sml = mkInitOption "Enable sml" [ ];
      solidity = mkInitOption "Enable do you need blockchain? No." [ ];
      swift = mkInitOption "Enable who asked for emoji variables?" [
        "lsp"
        "tree-sitter"
      ];
      terra = mkInitOption "Enable Earch and Moon in alignment for performance." [ ];
      web = mkInitOption "Enable the tubes" [
        "lsp"
        "tree-sitter"
      ];
      yaml = mkInitOption "Enable JSON, but readable" [
        "lsp"
        "tree-sitter"
      ];
      zig = mkInitOption "Enable C, but simpler" [
        "lsp"
        "tree-sitter"
      ];
    };
    email = {
      mu4e = mkInitOption "Enable mu4e" [
        "gmail"
        "org"
      ];
      notmuch = mkInitOption "Enable notmuch" [
        "afew"
        "org"
      ];
      wanderlust = mkInitOption "Enable wanderlust" [
        "gmail"
        "xface"
      ];
    };
    app = {
      calendar = mkInitOption "Enable calendar" [ ];
      emms = mkInitOption "Enable emms" [ ];
      everywhere = mkInitOption "Enable *leave* Emacs!? You must be joking" [ ];
      irc = mkInitOption "Enable how neckbeards socialize" [ ];
      rss = mkInitOption "Enable emacs as an RSS reader" [ "org" ];
    };
    config = {
      literate = mkInitOption "Enable literate" [ ];
      default = mkInitOption "Enable default" [
        "bindings"
        "smartparens"
      ];
    };
  };

  config = mkIf cfg.enable {
    file."init.el" = {
      enable = true;
      text = ''
        ;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

        ;; Generated by the ~/.myconf/modules/home-manger/programs/emacs/doom-init-type.nix file.

        ;; Additional setq values.

        ${
          if (length (attrNames cfg.setq)) == 0 then
            ";; (no setq values provided...)"
          else
            elisp.toSetq cfg.setq
        }

        ;; Extra config.

        ${cfg.extraConfig}

        ;; Doom init config.

        (doom! ${builtins.concatStringsSep "\n       " doomInitArgs})
      '';
    };
  };
}
