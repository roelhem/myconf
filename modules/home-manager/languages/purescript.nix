{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.purescript;

  npkgs = config.programs.nodejs.package.pkgs;

  purescript = cfg.package;
  purescript-language-server = cfg.lsp.package;
  purs-tidy = cfg.purs-tidy.package;

in
{

  options.languages.purescript = {
    enable = mkEnableOption "{command}`purescript`.";

    package = mkOption {
      type = types.package;
      default = pkgs.purescript;
      description = "The purescript package.";
    };

    purs-tidy.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    purs-tidy.package = mkOption {
      type = types.package;
      default = npkgs.purs-tidy;
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    lsp.package = mkOption {
      type = types.package;
      default = npkgs.purescript-language-server;
    };
  };

  config = {
    home.packages =
      optional cfg.enable purescript
      ++ optional cfg.lsp.enable purescript-language-server
      ++ optional cfg.purs-tidy.enable purs-tidy;

    programs.emacs.setq = {
      lsp-purescript-server-executable = "${purescript-language-server}/bin/purescript-language-server";
    };
  };

}
