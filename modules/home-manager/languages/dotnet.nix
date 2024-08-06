{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.languages.dotnet;

  dotnet = with pkgs.dotnetCorePackages; combinePackages [
    sdk_6_0
    sdk_7_0
    sdk_8_0
  ];

  omnisharp = pkgs.omnisharp-roslyn;
in
  {
    options.languages.dotnet = {
      enable = mkEnableOption "{command}`dotnet`";

      lsp.enable = mkEnableOption "{command}.`dotnet` language server";

    };

    config = {
      home.packages = optional cfg.enable dotnet;

      home.file."/Users/roel/nix-test/emacs/lsp-config.el" = mkIf cfg.lsp.enable {
        text = ''
          (setq lsp-csharp-omnisharp-roslyn-binary-path "${omnisharp}/bin/OmniSharp")
          '';
      };
    };
  }
