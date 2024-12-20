{
  config,
  lib,
  ...
}:

let

  inherit (lib) mkEnableOption mkIf;

  cfg = config.programs.arduino-ide;

in

{
  options.programs.arduino-ide = {
    enable = mkEnableOption "Arduino IDE";
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "arduino-ide" ];
    };
  };
}
