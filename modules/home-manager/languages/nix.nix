{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.languages.nix;

  homePackages = optional cfg.enable pkgs.nil;

in {

  options.languages.nix = {
    enable = mkEnableOption "{command}`nix` language";
    lsp.enable = mkEnableOption "{command}`nix` language server";
  };

  config = {
    home.packages = homePackages;
  };

}
