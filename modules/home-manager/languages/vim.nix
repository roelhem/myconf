{ config, lib, ... }:

with lib;

let

  cfg = config.languages.vim;

  npkgs = config.programs.nodejs.package.pkgs;

  vim-language-server = cfg.lsp.package;

in
{

  options.languages.vim = {
    enable = mkEnableOption "{command} `vim` language support.";

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`vim-language-server`.";
    };
    lsp.package = mkOption {
      type = types.package;
      default = npkgs.vim-language-server;
      description = "The `vim-language-server` package to use.";
    };
  };

  config = {
    home.packages = optional cfg.lsp.enable vim-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-vim-server-command = [
        "${vim-language-server}/bin/vim-language-server"
        "--stdio"
      ];
    };
  };

}
