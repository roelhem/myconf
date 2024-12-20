{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkDefault
    mkIf
    optional
    ;

  cfg = config.languages.rust;
in

{
  options.languages.rust = {
    enable = mkEnableOption "{command}`rust`";

    lsp.enable = mkEnableOption "{command}`rust-analyzer`";
  };

  config = {
    home.packages = optional cfg.enable pkgs.rustup;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-rust-analyzer-server-command = [ "${pkgs.rust-analyzer}/bin/rust-analyzer" ];
    };

    programs.emacs.doomConfig.init.lang.rust = {
      enable = mkDefault cfg.enable;
      lsp = mkDefault cfg.lsp.enable;
    };
  };
}
