{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.haskell;

  ghc = cfg.ghc.package;

  hpkgs =
    let
      ghckey =
        builtins.replaceStrings
          [
            "-"
            "."
          ]
          [
            ""
            ""
          ]
          ghc.name;
    in
    pkgs.haskell.packages.${ghckey};

  cabal = cfg.cabal.package;
  stack = cfg.stack.package;
  hls = cfg.lsp.package;
  hasktags = cfg.hasktags.package;
  hlint = cfg.hlint.package;
  hoogle = cfg.hoogle.package;
  fourmolu = cfg.fourmolu.package;
  stylish-haskell = cfg.stylish-haskell.package;

  haskellTools =
    if cfg.enable then
      [
        ghc
        cabal
        stack
        hasktags
        hlint
        hoogle
        fourmolu
        hpkgs.cabal-fmt
        # (hpkgs.cabal-hoogle.overrideAttrs (old: {
        #   buildInputs = old.buildInputs ++ [cabal];
        # }))
        hpkgs.hiedb
        hpkgs.hie-bios
        hpkgs.implicit-hie
        hpkgs.hpack
        hpkgs.ormolu
        hpkgs.floskell
        hpkgs.retrie
        hpkgs.hindent
        hpkgs.c2hs
        hpkgs.c2hsc
      ]
    else
      [ ];

  haskellDebugTools =
    if cfg.enable && cfg.debug.enable then
      [
        hpkgs.ghci-dap
        hpkgs.haskell-debug-adapter
      ]
    else
      [ ];

in
{
  options.programs.ihaskell = {
    enable = mkEnableOption "{command}`ihaskell`";

    package = mkOption {
      type = types.package;
      default = hpkgs.ihaskell;
    };
  };

  options.languages.haskell = {
    enable = mkEnableOption "{command}`haskell`";

    ghc.package = mkOption {
      type = types.package;
      default = pkgs.ghc;
      description = "The default GHC version.";
    };

    ghc.packages = mkOption {
      internal = true;
      visible = false;
      type = types.attrsOf types.package;
      default = hpkgs;
      description = "The haskell packages.";
    };

    cabal.package = mkOption {
      type = types.package;
      default = hpkgs.cabal-install;
      description = "The default cabal package";
    };

    stack.package = mkOption {
      type = types.package;
      default = hpkgs.stack;
    };

    hlint.package = mkOption {
      type = types.package;
      default = hpkgs.hlint;
    };

    hoogle.package = mkOption {
      type = types.package;
      default = hpkgs.hoogle;
    };

    hasktags.package = mkOption {
      type = types.package;
      default = hpkgs.hasktags;
    };

    fourmolu.package = mkOption {
      type = types.package;
      default = hpkgs.fourmolu;
    };

    stylish-haskell.package = mkOption {
      type = types.package;
      default = hpkgs.stylish-haskell;
    };

    debug.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable Haskell debug chain";
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable Haskell language server debug chain";
    };

    lsp.package = mkOption {
      type = types.package;
      default = hpkgs.haskell-language-server;
    };
  };

  config = {
    home.packages =
      haskellTools
      ++ haskellDebugTools
      ++ optional config.programs.ihaskell.enable config.programs.ihaskell.package
      ++ optional cfg.lsp.enable hls;

    programs.emacs.setq = mkIf cfg.enable {
      haskell-process-path-ghci = "${ghc}/bin/ghci";
      flycheck-haskell-ghc-executable = "${ghc}/bin/ghc";
      haskell-compile-command = "${ghc}/bin/ghc -Wall -ferror-spans -fforce-recomp -c %s";
      haskell-hasktags-path = "${hasktags}/bin/hasktags";
      haskell-hoogle-command = "${hoogle}/bin/hoogle --count=40";
      haskell-process-path-cabal = "${cabal}/bin/cabal";
      haskell-compile-cabal-build-command = "${cabal}/bin/cabal build --ghc-options=-ferror-spans";
      haskell-compile-cabal-build-alt-command = "${cabal}/bin/cabal clean -s && ${cabal}/bin/cabal build --ghc-options=-ferror-spans";
      haskell-process-path-stack = "${stack}/bin/stack";
      haskell-compile-stack-build-command = "${stack}/bin/stack build --fast";
      haskell-compile-stack-build-alt-command = "${stack}/bin/stack clean && ${stack}/bin/stack build --fast";
      flycheck-haskell-stack-ghc-executable = "${stack}/bin/stack";
      lsp-haskell-server-path = "${hls}/bin/haskell-language-server-wrapper";
      lsp-haskell-formatting-provider = "${fourmolu}/bin/fourmolu";
      haskell-mode-stylish-haskell-path = "${stylish-haskell}/bin/stylish-haskell";
      haskell-check-command = "${hlint}/bin/hlint";
      flycheck-haskell-hlint-executable = "${hlint}/bin/hlint";
    };
  };
}
