{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.languages.bicep;

  bicep = cfg.package;
  bicep-langserver = cfg.lsp.package;
in
{
  options.languages.bicep = {
    enable = mkEnableOption "{command}`bicep` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.bicep;
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.bicep-langserver;
      };
    };
  };

  config = {
    home.packages = optional cfg.enable bicep;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-bicep-langserver-path = "${bicep-langserver}/bin/Bicep.LangServer";
    };

    programs.emacs.extraPackages =
      epkgs: optional cfg.enable epkgs.bicep-ts-mode ++ optional cfg.lsp.enable epkgs.lsp-bicep;
  };

}
