args@{
  config,
  lib,
  pkgs,
  npkgs,
  inputs,
  myconf-literate-config,
  ...
}:

with lib;

let
  cfg = config.programs.emacs;
  emacs = cfg.finalPackage;
  doom = cfg.doomConfig.finalDoomPackage;

  emacs-zsh-plugin = pkgs.emacs-zsh-plugin;
  emacs-launch-editor = pkgs.emacs-launch-editor.override { inherit emacs; };
in
{

  options = {
    programs.emacs = {
      binaries = mkOption {
        type = with types; attrsOf string;
        description = "Sets the values for the `rh-nix-programs` variable.";
      };

      setq = mkOption {
        type = with types; attrsOf anything;
        description = "Set variables at the beginning of the config.el file. Values can be normal nix values.";
        default = { };
      };

      doomConfig = mkOption {
        type = lib.myconf.types.dir {
          modules = [
            {
              _module.args = args;
              name = "doom.d";
            }
            ./doom-config-type.nix
          ];
        };
      };

      enableZshIntegration = mkOption {
        type = types.bool;
        description = "Enable zsh integration.";
        default = cfg.enable;
      };
    };
  };

  config = {
    home.sessionVariables = {
      EDITOR = "${emacs}/bin/emacsclient";
      LAUNCH_EDITOR = "${emacs-launch-editor}";
    };

    home.packages = mkIf cfg.enable (
      [
        doom
        pkgs.coreutils-prefixed
        pkgs.libtool
        pkgs.fontconfig
        pkgs.stylelint
        pkgs.jsbeautifier
        pkgs.idris
      ]
      ++ myconf-literate-config.results.homePackages
    );

    programs.zsh = mkIf cfg.enableZshIntegration {
      initExtra = emacs-zsh-plugin.activationScript;
    };

    programs.emacs = {
      package = pkgs.emacs-plus;
      setq =
        mkIf config.programs.fd.enable {
          doom-projectile-fd-binary = "${config.programs.fd.package}/bin/fd";
          projectile-fd-executable = "${config.programs.fd.package}/bin/fd";
        }
        // mkIf config.programs.direnv.enable {
          envrc-direnv-executable = "${config.programs.direnv.package}/bin/direnv";
        }
        // {
          lsp-pwsh-exe = "${pkgs.powershell}/bin/pwsh";
          # lsp-sourcekit-executable = "${pkgs.sourcekit-lsp}/bin/sourcekit-lsp";
          lsp-rust-rustfmt-path = "${pkgs.rustfmt}/bin/rustfmt";
        };
      extraPackages = myconf-literate-config.results.emacs.elispPackages;
      doomConfig = myconf-literate-config.results.emacs.doom.buildDoomDir {
        inherit emacs lib;
        themesSource = ../../../../pkgs/doomemacs/themes;
        setq = cfg.setq;
        homeDirectory = config.home.homeDirectory;
      };
    };

    home.activation = {
      doomSync = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        ${doom}/bin/doom sync -e
      '';
    };

    launchd.agents.emacs = {
      enable = true;
      config = {
        Label = "emacs";
        ProgramArguments = [
          "${emacs}/bin/emacs"
          "--fg-daemon"
          "--init-directory=${cfg.doomConfig.doomPackage}"
        ];
        ProcessType = "Interactive";
        EnvironmentVariables =
          config.home.sessionVariables
          // cfg.doomConfig.envVariables
          // {
            HOSTNAME = "home-studio.local";
            HOME = "${config.home.homeDirectory}";
            USER = "${config.home.username}";
            NAME = "Roel Hemerik";
            LOGNAME = "${config.home.username}";
            EMAIL = "ik@roelweb.com";
            LANG = "nl_NL.UTF-8";
            PATH = "${config.home.homeDirectory}/.nix-profile/bin:/etc/profiles/per-user/${config.home.username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin";
            SHELL = "/bin/zsh";
            HISTFILE = "${config.home.homeDirectory}/.zsh_history";
            EDITOR = "${emacs}/bin/emacsclient";
            TMPDIR = "/var/folders/gp/9d4r9jss43z5yn2j0f6966bw0000gn/T/";
            TERMCAP = "${pkgs.termcap}";
            NIX_PATH = "nixpkgs=flake:nixpkgs:/nix/var/nix/profiles/per-user/root/channels";
            NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
            NIX_USER_PROFILE_DIR = "/nix/var/nix/profiles/per-user/${config.home.username}";
            NIX_PROFILES = "/nix/var/nix/profiles/default /run/current-system/sw /etc/profiles/per-user/${config.home.username} ${config.home.homeDirectory}/.nix-profile";
            NIX_REMOTE = "daemon";
            TERMINFO_DIRS = "${config.home.homeDirectory}/.nix-profile/share/terminfo:/etc/profiles/per-user/${config.home.username}/share/terminfo:/run/current-system/sw/share/terminfo:/nix/var/nix/profiles/default/share/terminfo:/usr/share/terminfo";
            XDG_CONFIG_DIRS = "${config.home.homeDirectory}/.nix-profile/etc/xdg:/etc/profiles/per-user/${config.home.username}/etc/xdg:/run/current-system/sw/etc/xdg:/nix/var/nix/profiles/default/etc/xdg";
            XDG_DATA_DIRS = "${config.home.homeDirectory}/.nix-profile/share:/etc/profiles/per-user/${config.home.username}/share:/run/current-system/sw/share:/nix/var/nix/profiles/default/share";
          };
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/Users/roel/.local/emacs/logs/server.stdout.log";
        StandardErrorPath = "/Users/roel/.local/emacs/logs/server.stderr.log";
      };
    };
  };
}
