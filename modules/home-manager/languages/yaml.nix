{ config, lib, ... }:

with lib;

let

  cfg = config.languages.yaml;

  npkgs = config.programs.nodejs.package.pkgs;

  yaml-language-server = cfg.lsp.package;

in
{

  options.languages.yaml = {
    enable = mkEnableOption "{command} `yaml` language support.";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`yaml-language-server`.";
    };
    lsp.package = mkOption {
      type = types.package;
      default = npkgs.yaml-language-server;
      description = "The `yaml-language-server` package to use";
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable yaml-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-yaml-server-command = [
        "${yaml-language-server}/bin/yaml-language-server"
        "--stdio"
      ];
    };

    programs.emacs.doomConfig.init.lang.yaml = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };

  };

}
