{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.scheme;

  mkSchemeOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  chez = cfg.chez.package;
  guile = cfg.guile.package;

in
{
  options.languages.scheme = {
    enable = mkEnableOption "{command}`scheme` language support";

    chez = mkSchemeOptions pkgs.chez;
    guile = mkSchemeOptions pkgs.guile;
  };

  config = {
    home.packages = optional cfg.chez.enable chez ++ optional cfg.guile.enable guile;
  };
}
