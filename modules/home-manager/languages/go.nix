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

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-gopls-server-path = "${gopls}/bin/gopls";
      lsp-go-gopls-server-path = "${gopls}/bin/gopls";
    };

  };
}
