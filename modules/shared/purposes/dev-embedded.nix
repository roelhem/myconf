{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib) mkEnableOption mkIf;

  cfg = config.purposes.dev.embedded;

in

{
  options.purposes.dev.embedded = {
    enable = mkEnableOption "Embedded Development";
  };

  config = mkIf cfg.enable {
    programs.arduino-ide.enable = true;
  };

  environment.systemPackages = [ pkgs.fritzing ];
}
