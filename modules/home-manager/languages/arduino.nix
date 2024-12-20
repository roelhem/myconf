{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib)
    mkOption
    mkIf
    mkDefault
    types
    optional
    ;

  cfg = config.languages.arduino;
in

{
  options.languages.arduino = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    arduino-mk = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.arduino-mk;
      };
    };

    arduino-ci = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.arduino-ci;
      };
    };

    arduino-cli = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.arduino-cli;
      };
    };

    arduino-create-agent = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.arduino-create-agent;
      };
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.arduino-language-server;
      };
    };

    bossa = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.bossa-arduino;
      };
    };

    platformio = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.platformio-core;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      optional cfg.arduino-mk.enable cfg.arduino-mk.package
      ++ optional cfg.arduino-ci.enable cfg.arduino-ci.package
      ++ optional cfg.arduino-cli.enable cfg.arduino-cli.package
      ++ optional cfg.arduino-create-agent.enable cfg.arduino-create-agent.package
      ++ optional cfg.lsp.enable cfg.lsp.package
      ++ optional cfg.bossa.enable cfg.bossa.package
      ++ optional cfg.platformio.enable cfg.platformio.package;

    languages.cc.enable = mkDefault true;
  };
}
