{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.faust;

  faust = cfg.package;

in
{

  options.languages.faust = {
    enable = mkEnableOption "{command} `faust` language support.";

    package = mkOption {
      type = types.package;
      default = pkgs.faust;
    };
  };

  config = {

    home.packages = optional cfg.enable faust;

    programs.emacs.doomConfig.init.lang.faust = {
      enable = cfg.enable;
    };

  };

}
