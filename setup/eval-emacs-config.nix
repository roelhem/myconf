{
  lib,
  pkgs,
  inputs ? { },
  modules ? [ ],
  baseModules ? [ ../modules/emacs ],
  specialArgs ? { },
}:

let

  argsModule = {
    _file = ./eval-emacs-config.nix;
    config = {
      _module.args = {
        inherit
          baseModules
          modules
          inputs
          pkgs
          ;
      };
    };
  };

  eval = lib.evalModules {
    class = "emacs";
    modules = modules ++ [ argsModule ] ++ baseModules;
    specialArgs = {
      modulesPath = builtins.toString ../modules/emacs;
    } // specialArgs;
  };

in

{
  inherit (eval._module.args) pkgs;
  inherit (eval) options config _module;
}
