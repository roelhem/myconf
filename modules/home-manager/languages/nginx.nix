{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.nginx;

  nginx-language-server = cfg.lsp.package;

in
{

  options.languages.nginx = {
    enable = mkEnableOption "{command} nginx config language support.";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`nginx-language-server`.";
    };

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.nginx-language-server;
      description = "The `nginx-language-server` package.";
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable nginx-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-nginx-server-command = "${pkgs.nginx-language-server}/bin/nginx-language-server";
    };
  };

}
