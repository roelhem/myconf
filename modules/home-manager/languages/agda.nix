{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  # hpkgs = config.languages.haskell.ghc.packages;
  cfg = config.languages.agda;

  agda = cfg.package;
  # agda2hs = cfg.agda2hs.package;

in
{

  options.languages.agda = {

    enable = mkEnableOption "{command} `agda` language support";

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };
    };

    package = mkOption {
      type = types.package;
      default = pkgs.agda;
    };

    # agda2hs = {
    #   enable = mkOption {
    #     type = types.bool;
    #     default = cfg.enable && config.languages.haskell.enable;
    #   };

    #   package = mkOption {
    #     type = types.package;
    #     default = hpkgs.agda2hs;
    #   };
    # };
  };

  config = {
    home.packages = optional cfg.enable agda; # ++ optional cfg.agda2hs.enable agda2hs;

    programs.emacs.doomConfig.init.lang.agda = {
      enable = cfg.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };
  };
}
