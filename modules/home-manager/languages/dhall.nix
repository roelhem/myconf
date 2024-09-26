{ config, lib, ... }:

with lib;

let

  cfg = config.languages.dhall;

  hpkgs = config.languages.haskell.ghc.packages;

  dhall = cfg.package;

  dhallHaskellPackageMap = {
    json = "dhall-json";
    yaml = "dhall-yaml";
    bash = "dhall-bash";
    toml = "dhall-toml";
    nix = "dhall-nix";
    nixpkgs = "dhall-nixpkgs";
    # csv = "dhall-csv";
    docs = "dhall-docs";
  };

  enabledDhallPackageNames = builtins.filter (name: cfg.${name}.enable) (
    builtins.attrNames dhallHaskellPackageMap
  );

  enabledDhallPackages = builtins.map (name: cfg.${name}.package) enabledDhallPackageNames;
in
{
  options.languages.dhall =
    {
      enable = mkEnableOption "{command}`dhall`";

      package = mkOption {
        type = types.package;
        default = hpkgs.dhall;
        description = "The dhall package to use.";
      };
    }
    // mapAttrs (name: packageName: {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
        description = "Enable `${packageName}`";
      };

      package = mkOption {
        type = types.package;
        default = hpkgs.${packageName};
        description = "The `${packageName}` package to use.";
      };
    }) dhallHaskellPackageMap;

  config = {
    home.packages = optional cfg.enable dhall ++ enabledDhallPackages;

    programs.emacs.setq = mkIf cfg.enable { dhall-command = "${dhall}/bin/dhall"; };
  };
}
