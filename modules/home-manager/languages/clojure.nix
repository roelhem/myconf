{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.clojure;

  mkClojureOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  clojure = cfg.package;
  clojure-lsp = cfg.lsp.package;
  neil = cfg.neil.package;
  jet = cfg.jet.package;
  clj-kondo = cfg.clj-kondo.package;
  cljfmt = cfg.cljfmt.package;

in
{

  options.languages.clojure = {
    enable = mkEnableOption "{command} `clojure`";

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    package = mkOption {
      type = types.package;
      default = pkgs.clojure;
    };

    lsp = mkClojureOptions pkgs.clojure-lsp;
    neil = mkClojureOptions pkgs.neil;
    jet = mkClojureOptions pkgs.jet;
    clj-kondo = mkClojureOptions pkgs.clj-kondo;
    cljfmt = mkClojureOptions pkgs.cljfmt;
  };

  config = {
    home.packages =
      optional cfg.enable clojure
      ++ optional cfg.lsp.enable clojure-lsp
      ++ optional cfg.neil.enable neil
      ++ optional cfg.jet.enable jet
      ++ optional cfg.clj-kondo.enable clj-kondo
      ++ optional cfg.cljfmt.enable cljfmt;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-clojure-custom-server-command = "${clojure-lsp}/bin/clojure-lsp";
    };

    programs.emacs.doomConfig.init.lang.clojure = {
      enable = cfg.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };
  };

}
