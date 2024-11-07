{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.dotnet;

  dotnet = cfg.package;
  mono = cfg.mono.package;
  omnisharp = cfg.lsp.package;
  csharpier = cfg.csharpier.package;
  fsharp = cfg.fsharp.package;

in
{
  options.languages.dotnet = {
    enable = mkEnableOption "{command}`dotnet`";

    package = mkOption {
      type = types.package;
      default =
        with pkgs.dotnetCorePackages;
        combinePackages [
          sdk_6_0
          sdk_7_0
          sdk_8_0
        ];
    };

    mono.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    mono.package = mkOption {
      type = types.package;
      default = pkgs.mono;
    };

    csharp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      tree-sitter = {
        enable = mkOption {
          type = types.bool;
          default = false;
        };
      };
    };

    fsharp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.fsharp;
      };

      tree-sitter = {
        enable = mkOption {
          type = types.bool;
          default = false;
        };
      };
    };

    csharpier.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    csharpier.package = mkOption {
      type = types.package;
      default = pkgs.csharpier;
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable `omnisharp-roslyn` csharp language server";
    };

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.omnisharp-roslyn;
    };

  };

  config = {
    home.packages =
      optional cfg.enable dotnet
      ++ optional cfg.lsp.enable omnisharp
      ++ optional cfg.csharpier.enable csharpier
      ++ optional cfg.fsharp.enable fsharp
      ++ optional cfg.mono.enable mono;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-csharp-omnisharp-roslyn-binary-path = "${omnisharp}/bin/OmniSharp";
    };

    programs.emacs.extraPackages =
      epkgs: optional cfg.fsharp.enable epkgs.ob-fsharp ++ optional cfg.fsharp.enable epkgs.eglot-fsharp;

    programs.emacs.doomConfig.init.lang = {
      csharp = {
        enable = cfg.csharp.enable;
        tree-sitter = cfg.csharp.tree-sitter.enable;
      };
      fsharp = {
        enable = cfg.fsharp.enable;
      };
    };
  };
}
