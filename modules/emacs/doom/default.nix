{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    types
    myconf
    mkOption
    mkEnableOption
    mkIf
    mkForce
    ;

  config' = config;

  doomModule =
    { config, name, ... }:
    {

      options = {
        enable = mkEnableOption "doomemacs";

        package = mkOption {
          type = types.package;
          default = inputs.doomemacs;
          description = ''
            The version of doom to use. This package will be used as the emacs directory
            if doom.enable is true.
          '';
        };

        envVariables = mkOption {
          type = types.attrsOf types.str;
          description = "The environment variables that will be added if doom.enable is true.";
          default = { };
        };

        finalPackage = mkOption {
          type = types.package;
          visible = false;
        };
      };

      config = {
        finalPackage = pkgs.writeShellApplication {
          name = "doom";
          runtimeInputs = config'.packages ++ [
            config.package
            pkgs.git
          ];
          runtimeEnv = config'.envVariables;
          text = ''
            doom "$@"
          '';
        };
      };
    };

  doomType = myconf.types.dir {
    specialArgs = {
      inherit (config) name;
    };
    modules = [
      {
        _module.args = {
          inherit pkgs inputs;
        };
        name = "${config.name}-doom.d";
      }
      doomModule
      ./init.nix
      ./environment.nix
    ];
  };

in

{
  options.doom = mkOption {
    type = doomType;
    description = "The configuration when using doom emacs.";
    default = { };
  };

  config = mkIf config.doom.enable {
    initDirectory = mkForce config.doom.package;
    packages = [ config.doom.finalPackage ];
    envVariables = config.doom.envVariables;
  };
}
