{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib)
    mkOption
    types
    literalExpression
    hm
    mkIf
    ;

  cfg = config.emacs;

  emacsPackages =
    let
      epkgs = pkgs.emacsPackagesFor cfg.package;
    in
    epkgs.overrideScope cfg.overrides;

  emacsWithPackages = emacsPackages.emacsWithPackages;

  extraPackages =
    epkgs:
    let
      packages = cfg.extraPackages epkgs;
    in
    packages;

in

{
  options = {
    emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to use a dedicated emacs build for this config instead of
          the default one.
        '';
      };

      package = mkOption {
        type = types.packages;
        default = pkgs.emacs;
        defaultText = literalExpression "pkgs.emacs";
        description = "The base emacs package used to run this config.";
      };

      extraPackages = mkOption {
        default = self: [ ];
        type = hm.types.selectorFunction;
      };

      overrides = mkOption {
        type = hm.types.overlayFunction;
        default = final: prev: { };
      };

      finalPackage = mkOption {
        type = types.package;
        visible = false;
        readOnly = true;
      };
    };
  };

  config = mkIf cfg.enable {
    emacs.finalPackage = emacsWithPackages extraPackages;

    packages = [ pkgs.finalPackage ];

    envVariables = {
      EMACS = "${cfg.finalPackage}/bin/emacs";
    };
  };
}
