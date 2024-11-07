{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.cc;

  mkCcOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  gcc = cfg.gcc.package;
  clang = cfg.clang.package;
  ccls = cfg.ccls.package;

in
{
  options.languages.cc = {
    enable = mkEnableOption "{command} `c` compiler support";

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    gcc = mkCcOptions pkgs.gcc;
    clang = mkCcOptions pkgs.libclang;
    ccls = mkCcOptions pkgs.ccls;

  };

  config = {
    home.packages =
      optional cfg.gcc.enable gcc
      ++ optional cfg.clang.enable clang
      ++ optional cfg.ccls.enable ccls
      ++ optional cfg.enable pkgs.glslang;

    programs.emacs.setq = mkIf cfg.enable { lsp-clangd-binary-path = "${clang}/bin/clangd"; };

    programs.emacs.doomConfig.init.lang.cc = {
      enable = cfg.enable;
      tree-sitter = cfg.enable;
    };
  };
}
