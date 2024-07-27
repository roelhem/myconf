{ config, lib, pkgs, ... }:

{
  system.stateVersion = 4;

  environment = {
    systemPackages = (with pkgs; [
      curl
      vim
    ]);

    systemPath = [];

    etc = {};

    launchAgents = {};

    userLaunchAgents = {};

    launchDaemons = {};

    shellAliases = {};

    shells = (with pkgs; []);

    variables = {};
  };

  services = {
    nix-daemon.enable = true;
  };

  programs = {
    zsh.enable = true;
  };

  fonts.packages = (with pkgs; [

  ]);

  homebrew = {
    enable = true;
  };

  nix.settings = {
    experimental-features = "nix-command flakes";
  };
}
