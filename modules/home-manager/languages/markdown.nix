{ config, lib, ... }:

with lib;

let

  cfg = config.languages.markdown;

  npkgs = config.programs.nodejs.package.pkgs;

  unified-language-server = cfg.lsp.package;

in
{

  options.languages.markdown = {

    enable = mkEnableOption "Enable markdown language.";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable the `unified-language-server`.";
    };

    lsp.package = mkOption {
      type = types.package;
      default = npkgs.unified-language-server;
      description = "The `unified-language-server` package to use.";
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable unified-language-server;

    programs.pandoc.enable = cfg.enable;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-markdown-server-command = "${unified-language-server}/bin/unified-language-server";
    };
  };

}