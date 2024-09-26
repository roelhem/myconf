{ config, lib, ... }:

with lib;

let

  cfg = config.languages.dockerfile;

  npkgs = config.programs.nodejs.package.pkgs;

  docker-langserver = cfg.lsp.package;

in
{

  options.languages.dockerfile = {

    enable = mkEnableOption "{command} `dockerfile` language support";

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = npkgs.dockerfile-language-server-nodejs;
      };
    };
  };

  config = {

    home.packages = optional cfg.lsp.enable docker-langserver;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-dockerfile-language-server-command = [
        "${docker-langserver}/bin/docker-langserver"
        "--stdio"
      ];
    };

  };

}
