{ pkgs, ... }:

{
  nix = {
    linux-builder.enable = true;

    settings = {
      max-jobs = "auto";
    };

    gc = {
      user = "root";
      automatic = true;
      interval = {
        Weekday = 0;
        Hour = 4;
        Minute = 0;
      };
      options = "--delete-older-than 30d";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      utm
      dosbox-x
      enchant
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
          nl
          fr
        ]
      ))
    ];
  };

  services = {
    nix-daemon.enable = true;
    yabai.enable = false;
  };

  purposes = {
    research.enable = true;
    dev.frontend.enable = true;
    dev.embedded.enable = true;
  };

  programs = {
    bash.enable = true;
    zsh = {
      enable = true;
      enableSyntaxHighlighting = true;
    };

    nix-index.enable = true;
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    global = { };
    casks = [
      "docker"
      "microsoft-teams"
      "microsoft-azure-storage-explorer"
      "plex-media-server"
      "pgadmin4"
    ];
    caskArgs = { };
    masApps = { };
    whalebrews = [ ];
  };

  launchd = { };
  networking = { };

  users = {
    users.roel = {
      name = "roel";
      description = "Roel Hemerik";
      home = "/Users/roel";
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      roel = import ../../../home/roel.nix;
    };
  };

  security = {
    pki.certificateFiles = [ ];
  };

  system = {
    stateVersion = 4;

    defaults = {
      # CustomUserPreferences = {
      #   "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" = true;
      # };

      dock = {
        autohide = true;
        expose-animation-duration = 0.5;
        orientation = "bottom";
        persistent-apps = [
          "${pkgs.iterm2}/Applications/iTerm2.app"
          "/Applications/Chromium.app"
          "/Applications/Safari.app"
          "/System/Applications/Mail.app"
          "/System/Applications/Passwords.app"
          "/Applications/Docker.app"
        ];
        persistent-others = [ "/Users/roel/Downloads" ];
        static-only = false;
      };
    };
  };

}
