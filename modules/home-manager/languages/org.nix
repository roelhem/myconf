{ config, lib, ... }:

with lib;

let

  cfg = config.languages.org;

in
{
  options.languages.org = {
    enable = mkEnableOption "{command} `org` language support.";

    brain = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    contacts = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    crypt = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    gnuplot = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    hugo = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    journal = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    jupyter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    noter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    pandoc = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    passwords = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    pomodoro = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    present = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    pretty = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    roam = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      version = mkOption {
        type = types.enum [
          1
          2
        ];
        default = 2;
      };
    };
  };

  config = {
    programs.emacs.doomConfig.init.lang.org = {
      enable = mkDefault cfg.enable;
      brain = mkDefault cfg.brain.enable;
      contacts = mkDefault cfg.contacts.enable;
      dragndrop = mkDefault cfg.dragndrop.enable;
      crypt = mkDefault cfg.crypt.enable;
      gnuplot = mkDefault cfg.gnuplot.enable;
      hugo = mkDefault cfg.hugo.enable;
      journal = mkDefault cfg.journal.enable;
      jupyter = mkDefault cfg.jupyter.enable;
      noter = mkDefault cfg.noter.enable;
      pandoc = mkDefault cfg.pandoc.enable;
      passwords = mkDefault cfg.passwords.enable;
      pomodoro = mkDefault cfg.pomodoro.enable;
      present = mkDefault cfg.present.enable;
      pretty = mkDefault cfg.pretty.enable;
      roam = mkDefault (cfg.roam.enable && cfg.roam.version == 1);
      roam2 = mkDefault (cfg.roam.enable && cfg.roam.version == 2);
    };
  };
}
