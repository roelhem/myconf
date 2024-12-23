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
        default = config.programs.arduino-cli.package;
      };

      configurationFile = mkOption {
        type = types.path;
        default = config.programs.arduino-cli.configurationFile;
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

      clang.package = mkOption {
        type = types.package;
        default = config.languages.cc.clang.package;
      };

      fqbn = mkOption {
        type = types.str;
        default = "arduino:avr:uno";
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
      ++ optional cfg.arduino-create-agent.enable cfg.arduino-create-agent.package
      ++ optional cfg.lsp.enable cfg.lsp.package
      ++ optional cfg.bossa.enable cfg.bossa.package
      ++ optional cfg.platformio.enable cfg.platformio.package;

    languages.cc.enable = mkDefault true;

    programs.arduino-cli.enable = mkDefault cfg.arduino-cli.enable;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-arduino-server-binary-path = "${cfg.lsp.package}/bin/arduino-language-server";
      lsp-arduino-clangd-binary-path = "${cfg.lsp.clang.package}/bin/clangd";
      lsp-arduino-cli-binary-path = "${cfg.arduino-cli.package}/bin/arduino-cli";
      lsp-arduino-cli-config-file = "${cfg.arduino-cli.configurationFile}";
      lsp-arduino-cli-fqbn = "${cfg.lsp.fqbn}";
      lsp-arduino-server-command = [
        "${cfg.lsp.package}/bin/arduino-language-server"
        "-clangd"
        "${cfg.lsp.clang.package}/bin/clangd"
        "-cli"
        "${cfg.arduino-cli.package}/bin/arduino-cli"
        "-cli-config"
        "${cfg.arduino-cli.configurationFile}"
        "-fqbn"
        "${cfg.lsp.fqbn}"
      ];
    };

    programs.emacs.extraPackages =
      epkgs:
      optional cfg.platformio.enable epkgs.platformio-mode
      ++ optional cfg.arduino-cli.enable epkgs.arduino-cli-mode
      ++ [ epkgs.arduino-ts-mode ];
  };
}
