{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.sml;

  sml = cfg.package;

in
{

  options.languages.sml = {
    enable = mkEnableOption "{command} `sml` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.smlnj;
    };
  };

  config = {
    home.packages = optional cfg.enable sml;

    programs.emacs.doomConfig.init.lang.sml = {
      enable = cfg.enable;
    };
  };

}
