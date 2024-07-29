{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs;

    casks = optional cfg.chromium.enable       "chromium"
         ++ optional cfg.google-chrome.enable  "google-chrome"
         ++ optional cfg.opera.enable          "opera"
         ++ optional cfg.microsoft-edge.enable "microsoft-edge"
         ++ optional cfg.firefox.enable        "firefox";

in
  {
    options = {
      programs.chromium = {
        enable = options.mkEnableOption "{command}`chromium`";
      };

      programs.google-chrome = {
        enable = options.mkEnableOption "{command}`google-chrome`";
      };

      programs.opera = {
        enable = options.mkEnableOption "{command}`opera`";
      };

      programs.firefox = {
        enable = options.mkEnableOption "{command}`firefox`";
      };

      programs.microsoft-edge = {
        enable = options.mkEnableOption "{command}`microsoft-edge`";
      };
    };

    config = mkIf (length casks > 0) {
      homebrew = {
        enable = true;
        inherit casks;
      };
    };
  }
