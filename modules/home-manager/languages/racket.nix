{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.racket;

  racket = cfg.package;

in
{

  options.languages.racket = {
    enable = mkEnableOption "{command} `racket` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.racket;
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    xp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {

    home.packages = optional cfg.enable racket;

    programs.emacs.doomConfig.init.lang.racket = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      xp = cfg.xp.enable;
    };

  };
}
