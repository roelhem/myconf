{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkOption
    types
    optional
    mkIf
    ;

  cfg = config.languages.sql;

  sqls = cfg.lsp.package;
in
{
  options.languages.sql = {
    enable = mkEnableOption "{command} `sql` language support";

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.sqls;
      };
    };
  };

  config = {
    home.packages = optional (cfg.enable && cfg.lsp.enable) sqls ++ optional cfg.enable pkgs.postgresql;

    programs.emacs.doomConfig.init = {
      setq = mkIf (cfg.enable && cfg.lsp.enable) {
        lsp-sqls-server = "${sqls}/bin/sqls";
      };
    };
  };
}
