{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.nix;

  mkNixOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  nixfmt = cfg.nixfmt.package;
  nil = cfg.nil.package;
  nixd = cfg.nixd.package;

in
{

  options.languages.nix = {
    enable = mkEnableOption "{command}`nix` language";

    nixfmt = mkNixOptions pkgs.nixfmt-rfc-style;
    nil = mkNixOptions pkgs.nil;
    nixd = mkNixOptions pkgs.nixd;

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    home.packages =
      optional cfg.nil.enable nil ++ optional cfg.nixd.enable nixd ++ optional cfg.nixfmt.enable nixfmt;

    programs.emacs.setq =
      mkIf cfg.nixfmt.enable { nix-nixfmt-bin = "${nixfmt}/bin/nixfmt"; }
      // mkIf cfg.nixd.enable { lsp-nix-nixd-server-path = "${nixd}/bin/nixd"; };

    programs.emacs.doomConfig.init.lang.nix = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };
  };

}
