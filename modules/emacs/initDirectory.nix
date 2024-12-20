{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib)
    mkOption
    myconf
    types
    strings
    attrsets
    ;

  ensurePathEndsWithSlash =
    p:
    let
      path = builtins.toString p;
    in
    if strings.hasSuffix "/" path then path else path + "/";

  initDirectoryType = myconf.types.dir {
    storeNamePrefix = "emacs.d";
    specialArgs = {
      inherit lib;
    };
    modules = [
      (
        { ... }:
        {
          config = {
            _module.args = {
              inherit pkgs;
            };

            name = "${config.name}-emacs.d";
          };
        }
      )
    ];
  };

  initDirectory = config.initDirectory;

  # Computes the value for the --init-directory argument that can be
  # passed to emacs.
  initDirectoryPath =
    if initDirectory == null then
      null
    else if
      builtins.isString initDirectory
      || builtins.isPath initDirectory
      || attrsets.isDerivation initDirectory
      || builtins.hasAttr "outPath" initDirectory
    then
      builtins.toString initDirectory
    else
      builtins.toString initDirectory.finalDirPackage;
in

{

  options = {
    initDirectory = mkOption {
      type = with types; nullOr (either path initDirectoryType);
      default = null;
    };

    initDirectoryPath = mkOption {
      type = with types; nullOr str;
      internal = true;
    };
  };

  config = {
    initDirectoryPath = initDirectoryPath;

    envVariables = {
      EMACSDIR = ensurePathEndsWithSlash initDirectoryPath;
    };
  };

}
