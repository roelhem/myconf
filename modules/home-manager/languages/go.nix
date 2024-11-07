{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.go;

  gopls = cfg.lsp.package;

in
{
  options.languages.go = {
    enable = mkEnableOption "{command}`go`";

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable `gopls`";
    };

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.gopls;
      description = "The `gopls` package to use.";
    };
  };

  config = {
    programs.go.enable = cfg.enable;

    home.packages =
      optional cfg.lsp.enable cfg.lsp.package
      ++ optional cfg.enable pkgs.gotests
      ++ optional cfg.enable pkgs.gore
      ++ optional cfg.enable pkgs.gomodifytags;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-gopls-server-path = "${gopls}/bin/gopls";
      lsp-go-gopls-server-path = "${gopls}/bin/gopls";
    };

    programs.emacs.doomConfig.init.lang.go = {
      enable = mkDefault cfg.enable;
      lsp = mkDefault cfg.lsp.enable;
      tree-sitter = mkDefault cfg.tree-sitter.enable;
    };

  };
}
