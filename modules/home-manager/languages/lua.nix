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

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`lua-language-server`";
    };

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.lua-language-server;
      description = "The `lua-language-server` package";
    };
  };

  config = {
    home.packages = optional cfg.enable lua ++ optional cfg.lsp.enable lua-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-clients-lua-language-server-bin = "${lua-language-server}/bin/lua-language-server";
    };
  };

}
