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
  };

  config = {
    home.packages = optional cfg.enable julia;
  };
}
