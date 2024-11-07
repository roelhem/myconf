{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.ocaml;

  ocaml = cfg.package;

  mkToolOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  merlin = cfg.merlin.package;
  utop = cfg.utop.package;
  ocp-indent = cfg.ocp-indent.package;
  dune = cfg.dune.package;
  ocamlformat = cfg.ocamlformat.package;

  inherit (pkgs) ocamlPackages;

in
{

  options.languages.ocaml = {
    enable = mkEnableOption "{command}`ocaml` language support.";

    package = mkOption {
      type = types.package;
      default = pkgs.ocaml;
    };

    merlin = mkToolOptions ocamlPackages.merlin;
    utop = mkToolOptions ocamlPackages.utop;
    ocp-indent = mkToolOptions ocamlPackages.ocp-indent;
    dune = mkToolOptions pkgs.dune_3;
    ocamlformat = mkToolOptions ocamlPackages.ocamlformat;

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {

    home.packages =
      optional cfg.enable ocaml
      ++ optional cfg.merlin.enable merlin
      ++ optional cfg.utop.enable utop
      ++ optional cfg.ocp-indent.enable ocp-indent
      ++ optional cfg.dune.enable dune
      ++ optional cfg.ocamlformat.enable ocamlformat;

    programs.opam = mkIf cfg.enable { enable = true; };

    programs.emacs.doomConfig.init.lang.ocaml = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };
  };

}
