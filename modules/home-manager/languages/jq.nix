{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.jq;

  jq = cfg.package;
  jq-lsp = cfg.lsp.package;

in
{

  options.languages.jq = {
    enable = mkEnableOption "{command}`jq` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.jq;
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
        description = "Enable `jq-lsp`";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.jq-lsp;
        description = "The `jq-lsp` package";
      };
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable jq-lsp;

    programs.jq = mkIf cfg.enable {
      enable = true;
      package = jq;
    };

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-clients-jq-server-executable = [ "${jq-lsp}/bin/jq-lsp" ];
    };
  };

}
