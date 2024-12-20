{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib) mkOption types mkIf;

  cfg = config.purposes.dev.frontend;

in

{
  options.purposes.dev.frontend = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs = {
      chromium.enable = true;
      google-chrome.enable = true;
      opera.enable = true;
      firefox.enable = true;
    };
  };
}
