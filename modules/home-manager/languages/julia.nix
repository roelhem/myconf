{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.julia;

  julia = cfg.package;

in
{
  options.languages.julia = {
    enable = mkEnableOption "{command} `julia` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.julia-bin;
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    snail = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    home.packages = optional cfg.enable julia;

    programs.emacs.doomConfig.init.lang.julia = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
      snail = cfg.snail.enable;
    };
  };
}
