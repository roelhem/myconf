{ config, lib, ... }:

with lib;

let
  cfg = config.languages.javascript;

  nodejs = config.programs.nodejs.package;
  npkgs = nodejs.pkgs;

  typescript = cfg.typescript.package;
  typescript-language-server = cfg.typescript.lsp.package;
  eslint = cfg.eslint.package;
  prettier = cfg.prettier.package;

in
{
  options.languages.javascript = {
    enable = mkEnableOption "{command} `javascript` language support.";

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    typescript = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = npkgs.typescript;
        description = "The package containing the typescript compiler.";
      };

      lsp = {
        enable = mkOption {
          type = types.bool;
          default = cfg.typescript.enable;
          description = "Enable the typescript language server.";
        };

        package = mkOption {
          type = types.package;
          default = npkgs.typescript-language-server;
        };
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    eslint = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = npkgs.eslint;
      };
    };

    prettier = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = npkgs.prettier;
      };
    };
  };

  config = {
    home.packages =
      optional cfg.typescript.enable typescript
      ++ optional cfg.typescript.lsp.enable typescript-language-server
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

    programs.emacs.doomConfig.init.lang.javascript = mkIf cfg.enable {
      enable = mkDefault cfg.enable;
      lsp = mkDefault cfg.lsp.enable;
      tree-sitter = mkDefault cfg.tree-sitter.enable;
    };
  };
}
