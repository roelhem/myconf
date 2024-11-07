{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.coq;

  coq = cfg.package;

in
{
  options.languages.coq = {
    enable = mkEnableOption "{enable} `coq` language support.";

    package = mkOption {
      type = types.package;
      default = pkgs.coq;
    };
  };

  config = {
    home.packages = optional cfg.enable coq;

    programs.emacs.doomConfig.init.lang.coq = {
      enable = cfg.enable;
    };
  };
}
