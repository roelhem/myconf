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
    mkIf
    optional
    ;

  cfg = config.languages.protobuf;

in

{
  options.languages.protobuf = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.protobuf;
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.emacs.extraPackages =
      epkgs: [ epkgs.protobuf-mode ] ++ optional cfg.tree-sitter.enable epkgs.protobuf-ts-mode;
  };
}
