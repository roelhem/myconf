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
    systemPackages = with pkgs; [ utm ];
  };

  services = {
    nix-daemon.enable = true;
    yabai = {
      enable = true;
      config = {
        layout = "bsp";
      };
      extraConfig = ''
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add app="^System Information$" manage=off
        yabai -m rule --add app="^System Preferences$" manage=off
        yabai -m rule --add app="Preferences$" manage=off
        yabai -m rule --add app="Settings$" manage=off
        yabai -m rule --add app="Systeeminstellingen" manage=off
      '';
    };
  };

  programs = {
    bash.enable = true;
    zsh = {
      enable = true;
      enableSyntaxHighlighting = true;
    };

    nix-index.enable = true;
    info.enable = true;
    man.enable = true;

    chromium.enable = true;
    google-chrome.enable = true;
    opera.enable = true;
    firefox.enable = true;
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    global = { };
    casks = [
      "docker"
      "microsoft-azure-storage-explorer"
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
      NSGlobalDomain = {
        # Mouse
        AppleEnableMouseSwipeNavigateWithScrolls = false;
        AppleEnableSwipeNavigateWithScrolls = false;

        # Keyboard
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticInlinePredictionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        KeyRepeat = 2;
        InitialKeyRepeat = 15;

        # Scrolling
        AppleShowScrollBars = "Automatic";
        AppleScrollerPagingBehavior = true;

        # Finder
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;

        # Units
        AppleMeasurementUnits = "Centimeters";
        AppleMetricUnits = 1;
        AppleTemperatureUnit = "Celsius";
      };

      # CustomUserPreferences = {
      #   "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" = true;
      # };

      alf = {
        allowdownloadsignedenabled = 1;
        allowsignedenabled = 1;
      };

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
        persistent-others = [ "~/Downloads" ];
        show-process-indicators = true;
        show-recents = true;
        static-only = false;
        tilesize = 64;
        wvous-bl-corner = 1;
        wvous-br-corner = 1;
        wvous-tl-corner = 1;
        wvous-tr-corner = 1;
      };

      finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        CreateDesktop = false;
        FXDefaultSearchScope = "SCcf";
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "Nlsv";
        ShowStatusBar = true;
        _FXShowPosixPathInTitle = true;
      };

      magicmouse.MouseButtonMode = "TwoButton";

      menuExtraClock = {
        Show24Hour = true;
        ShowAMPM = false;
        ShowDate = 0;
        ShowDayOfMonth = true;
        ShowDayOfWeek = true;
        ShowSeconds = false;
      };

      spaces.spans-displays = false;

      trackpad = {
        Clicking = false;
        Dragging = false;
        TrackpadRightClick = true;
      };

    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

}
