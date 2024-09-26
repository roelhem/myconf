{ config, lib, ... }:

with lib;

let

  npkgs = config.programs.nodejs.package.pkgs;
  cfg = config.languages.json;

  vscode-json-languageserver = cfg.lsp.package;

in
{

  options.languages.json = {
    enable = mkEnableOption "{command} `json` language support";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    lsp.package = mkOption {
      type = types.package;
      default = npkgs.vscode-json-languageserver;
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable vscode-json-languageserver;
  };

}
