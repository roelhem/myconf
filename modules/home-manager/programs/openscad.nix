{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.programs.openscad;

  openscad = cfg.package;
  openscad-lsp = cfg.lsp.package;

in
{

  options.programs.openscad = {
    enable = mkEnableOption "{command}`openscad`";

    package = mkOption {
      type = types.package;
      default = pkgs.openscad;
      description = "The openscad package.";
    };

    lsp.enable = mkEnableOption "{command}`openscad-lsp`";

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.openscad-lsp;
    };
  };

  config = {
    home.packages = optional cfg.enable openscad ++ optional cfg.lsp.enable openscad-lsp;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-openscad-server = "${pkgs.openscad-lsp}/bin/openscad-lsp";
    };
  };

}
