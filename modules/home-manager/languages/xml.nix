{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.xml;

  lemminx = cfg.lsp.package;

in
{

  options.languages.xml = {
    enable = mkEnableOption "{command} `xml` language support.";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable `lemminx` xml language server";
    };
    lsp.package = mkOption {
      type = types.package;
      default = pkgs.lemminx;
      description = "The `lemminx` package";
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable lemminx;

    programs.emacs.setq = mkIf cfg.lsp.enable { lsp-xml-bin-file = "${pkgs.lemminx}/bin/lemminx"; };
  };

}
