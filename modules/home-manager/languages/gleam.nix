{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.gleam;

  gleam = cfg.package;

in
{

  options.languages.gleam = {
    enable = mkEnableOption "{command}`gleam`";
    package = mkOption {
      type = types.package;
      default = pkgs.gleam;
    };

    lsp.enable = mkEnableOption "{command}`gleam` lsp";
  };

  config = {

    home.packages = optional cfg.enable gleam;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-gleam-executable = [
        "${gleam}/bin/gleam"
        "lsp"
      ];
    };

  };

}
