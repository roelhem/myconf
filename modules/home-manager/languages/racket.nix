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
  };

  config = {

    home.packages = optional cfg.enable racket;

  };
}
