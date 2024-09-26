{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.kotlin;

  mkKotlinOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  kotlin = cfg.package;
  kotlin-language-server = cfg.lsp.package;
  ktlint = cfg.ktlint.package;

in

{
  options.languages.kotlin = {
    enable = mkEnableOption "{command}`kotlin` language support";

    package = mkOption {
      type = types.package;
      default = pkgs.kotlin;
    };

    ktlint = mkKotlinOptions pkgs.ktlint;
    lsp = mkKotlinOptions pkgs.kotlin-language-server;

  };

  config = {
    home.packages =
      optional cfg.enable kotlin
      ++ optional cfg.lsp.enable kotlin-language-server
      ++ optional cfg.ktlint.enable ktlint;
  };
}
