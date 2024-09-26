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

    fsharp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    fsharp.package = mkOption {
      type = types.package;
      default = pkgs.fsharp;
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
  };
}
