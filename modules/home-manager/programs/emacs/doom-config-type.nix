{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:

let
  inherit (lib)
    mkOption
    types
    ;
in

{
  imports = [ ./doom-config-init.nix ];

  options = {
    doomPackage = mkOption {
      type = types.package;
      description = "The source of doom emacs used as the 'normal' emacs dir.";
      default = inputs.doomemacs;
    };

    emacsPackage = mkOption {
      type = types.package;
      internal = true;
      description = "The final emacs package used to run this doom config.";
    };

    finalDoomPackage = mkOption {
      type = types.package;
      internal = true;
      description = "The final wrapped doom command.";
    };

    localDir = mkOption {
      type = types.str;
      description = "The directory for all the local caches made by doom emacs.";
    };

    doomDir = mkOption {
      type = types.str;
      internal = true;
    };

    envVariables = mkOption {
      type = types.attrsOf types.str;
    };
  };

  config = {
    doomDir = "${config.finalDirPackage}/";

    envVariables = {
      EMACS = "${config.emacsPackage}/bin/emacs";
      EMACSDIR = "${config.doomPackage}/";
      DOOMDIR = config.doomDir;
      DOOMLOCALDIR = config.localDir;
    };

    finalDoomPackage = pkgs.writeShellApplication {
      name = "doom";
      runtimeInputs = [
        config.emacsPackage
        config.doomPackage
        pkgs.git
      ];
      runtimeEnv = config.envVariables;
      text = ''
        doom "$@"
      '';
    };
  };
}
