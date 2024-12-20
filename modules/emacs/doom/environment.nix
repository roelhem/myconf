{
  config,
  lib,
  name,
  ...
}:

let

  inherit (lib)
    mkOption
    mkDefault
    types
    strings
    ;

  ensurePathEndsWithSlash =
    p:
    let
      path = builtins.toString p;
    in
    if strings.hasSuffix "/" path then path else path + "/";
in

{

  options = {
    localDir = mkOption {
      type = types.path;
      description = "The directory where doom emacs will store its caches.";
      default = "/etc/nix-emacs/profiles/${name}/doom/.local";
    };

    doomDir = mkOption {
      type = types.path;
      description = "The path to the doom config.";
    };
  };

  config = {
    doomDir = mkDefault config.finalDirPackage;

    envVariables = {
      DOOMDIR = ensurePathEndsWithSlash config.doomDir;
      DOOMLOCALDIR = ensurePathEndsWithSlash config.localDir;
    };
  };
}
