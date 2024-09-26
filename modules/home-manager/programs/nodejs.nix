{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.nodejs;

  nodejs = cfg.package;
  corepack = cfg.corepack.package;

  npkgs = nodejs.pkgs;
in
{
  options.programs.nodejs = {
    enable = mkEnableOption "Enable nodejs javascript runtime";

    package = mkOption {
      type = types.package;
      default = pkgs.nodejs;
      description = "The packages of the nodejs version to use.";
    };

    corepack.enable = mkEnableOption "Enable corepack";

    corepack.package = mkOption {
      type = types.package;
      default = pkgs.corepack;
      description = "The corepack package to use";
    };
  };

  config = {
    home.packages = optional cfg.enable nodejs ++ optional cfg.corepack.enable corepack;

    programs.emacs.setq = mkIf cfg.enable { lsp-clients-npm-location = "${nodejs}/bin/npm"; };
  };
}
