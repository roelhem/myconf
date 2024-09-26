{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.doomemacs;

  doomDir = cfg.doomDir;
  localDir = cfg.localDir;

  doomemacs = pkgs.doomemacs.override {
    inherit doomDir localDir;
    emacs = cfg.package;
  };

  doomConfig = pkgs.orgTangleFile ../../../../config.org { };

in
{
  options = {
    programs.doomemacs = {
      enable = mkEnableOption "Enable doomemacs";

      package = mkOption {
        type = types.package;
        default = config.programs.emacs.package;
        description = "The emacs package from which doom emacs will be derived.";
      };

      doomDir = mkOption {
        type = types.str;
        default = "${config.home.homeDirectory}/.doom.d";
        description = "The directory where the configuration lives.";
      };

      localDir = mkOption {
        type = types.str;
        default = "${doomDir}/.local";
        description = "Directory for all the local caches made by doom emacs.";
      };

      finalPackage = mkOption {
        type = types.package;
        visible = false;
        readOnly = true;
        description = ''
          The DoomEmacs package.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      doomemacs
      pkgs.coreutils-prefixed
    ] ++ (if pkgs.stdenv.isDarwin then [ pkgs.pngpaste ] else [ ]);

    home.activation = {
      doomSync = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p ${doomDir}
        cp -f ${doomConfig}/config.el ${doomDir}/config.el
        cp -f ${doomConfig}/packages.el ${doomDir}/packages.el

        ${doomemacs}/bin/doom sync
      '';
    };

    languages.cmake.enable = true;

    programs.doomemacs.finalPackage = doomemacs;
  };
}
