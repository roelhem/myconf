{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.languages.sh;

  shellcheck = cfg.shellcheck.package;
  shfmt = cfg.shfmt.package;
in
{
  options.languages.sh = {
    enable = mkEnableOption "{command} sh language support";

    shellcheck = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
      package = mkOption {
        type = types.package;
        default = pkgs.shellcheck;
      };
    };

    shfmt = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
      package = mkOption {
        type = types.package;
        default = pkgs.shfmt;
      };
    };
  };

  config = {
    home.packages = optional cfg.shellcheck.enable shellcheck ++ optional cfg.shfmt.enable shfmt;

    programs.emacs.setq = mkIf cfg.shellcheck.enable {
      sh-shellcheck-program = "${shellcheck}/bin/shellcheck";
    };
  };
}
