{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.zig;

  zig = cfg.package;
  zls = cfg.lsp.package;

in
{

  options.languages.zig = {
    enable = mkEnableOption "{command}`zig`";
    package = mkOption {
      type = types.package;
      default = pkgs.zig;
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
        description = "{command}`zls` zig language server.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.zls;
      };
    };
  };

  config = {

    home.packages = optional cfg.enable zig ++ optional cfg.lsp.enable zls;

    programs.emacs.setq = mkIf cfg.lsp.enable { lsp-zig-zls-executable = "${zls}/bin/zls"; };

    programs.emacs.doomConfig.init.lang.zig = {
      enable = cfg.enable;
      tree-sitter = cfg.tree-sitter.enable;
      lsp = cfg.lsp.enable;
    };

  };

}
