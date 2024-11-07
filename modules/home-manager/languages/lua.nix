{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.lua;

  lua = cfg.package;
  lua-language-server = cfg.lsp.package;

in
{

  options.languages.lua = {

    enable = mkEnableOption "{command} `lua` language support.";

    package = mkOption {
      type = types.package;
      default = pkgs.lua;
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
        description = "{command}`lua-language-server`";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.lua-language-server;
        description = "The `lua-language-server` package";
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    fennel = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    moonscript = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    home.packages = optional cfg.enable lua ++ optional cfg.lsp.enable lua-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-clients-lua-language-server-bin = "${lua-language-server}/bin/lua-language-server";
    };

    programs.emacs.doomConfig.init.lang.lua = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
      fennel = cfg.fennel.enable;
      moonscript = cfg.moonscript.enable;
    };
  };

}
