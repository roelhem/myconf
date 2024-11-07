{
  config,
  lib,
  npkgs,
  ...
}:

with lib;

let

  cfg = config.languages.graphql;

in
{
  options.languages.graphql = {
    enable = mkEnableOption "{command} `graphql` language support.";

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };
  };

  config = {
    home.packages = optional (cfg.enable && cfg.lsp.enable) npkgs.graphql-language-service-cli;

    programs.emacs.doomConfig.init.lang.graphql = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
    };
  };
}
