{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.idris2;

  idris2 = cfg.package;
  idris2-lsp = cfg.lsp.package;

in
{

  options.languages.idris2 = {
    enable = mkEnableOption "{command}`idris2`";
    package = mkOption {
      type = types.package;
      default = pkgs.idris2;
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable `idris2-lsp`";
    };
    lsp.package = mkOption {
      type = types.package;
      default = pkgs.idris2Packages.idris2Lsp;
    };
  };

  config = {

    home.packages = optional cfg.enable idris2 ++ optional cfg.lsp.enable idris2-lsp;

    programs.emacs.setq = mkIf cfg.lsp.enable { lsp-idris2-lsp-path = "${idris2-lsp}/bin/idris2-lsp"; };

  };

}
