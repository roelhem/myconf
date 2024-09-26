{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.toml;

  taplo = cfg.taplo.package;

in
{

  options.languages.toml = {
    enable = mkEnableOption "{command} `toml` support.";

    taplo.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`taplo`.";
    };
    taplo.package = mkOption {
      type = types.package;
      default = pkgs.taplo;
      description = "The `taplo` package to use";
    };
  };

  config = {
    home.packages = optional cfg.taplo.enable taplo;

    programs.emacs.setq = mkIf cfg.taplo.enable { lsp-toml-command = "${taplo}/bin/taplo"; };
  };

}
