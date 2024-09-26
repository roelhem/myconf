{ config, lib, ... }:

with lib;

let

  cfg = config.languages.typescript;

  nodejs = config.programs.nodejs.package;
  npkgs = nodejs.pkgs;

  typescript = cfg.package;
  typescript-language-server = cfg.lsp.package;
  eslint = cfg.eslint.package;
  prettier = cfg.prettier.package;

  mkTsOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };
    package = mkOption {
      type = types.package;
      default = package;
    };
  };

in
{
  options.languages.typescript = {
    enable = mkEnableOption "{command}`typescript`";

    package = mkOption {
      type = types.package;
      default = npkgs.typescript;
      description = "The package containing the typescript compiler";
    };

    lsp = mkTsOptions npkgs.typescript-language-server;
    eslint = mkTsOptions npkgs.eslint;
    prettier = mkTsOptions npkgs.prettier;
  };

  config = {
    home.packages =
      optional cfg.enable typescript
      ++ optional cfg.lsp.enable typescript-language-server
      ++ optional cfg.eslint.enable eslint
      ++ optional cfg.prettier.enable prettier;

    programs.nodejs = mkIf cfg.enable {
      enable = true;
      corepack.enable = true;
    };

    programs.emacs.setq =
      mkIf cfg.lsp.enable {
        lsp-clients-typescript-tls-path = "${typescript-language-server}/bin/typescript-language-server";
        lsp-typescript-npm = "${nodejs}/bin/npm";
      }
      // mkIf cfg.eslint.enable {
        lsp-eslint-package-manager = "${nodejs}/bin/npm";
        lsp-eslint-runtime = "${nodejs}/bin/node";
      }
      // {
        lsp-eslint-enable = cfg.eslint.enable;
      };
  };
}
