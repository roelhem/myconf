{
  lib,
  config,
  homebrew-core,
  homebrew-cask,
  homebrew-bundle,
  defaultUser,
  ...
}:

{
  imports = [
    ./programs/browsers.nix
    ./services/yabai.nix
  ];

  config = {

    nix-homebrew = {
      user = defaultUser.name;
      enable = config.homebrew.enable;
      taps = {
        "homebrew/homebrew-core" = homebrew-core;
        "homebrew/homebrew-cask" = homebrew-cask;
        "homebrew/homebrew-bundle" = homebrew-bundle;
      };
      mutableTaps = false;
      autoMigrate = true;
    };

    system = with lib; {
      defaults = {
        NSGlobalDomain = {
          # Mouse
          AppleEnableMouseSwipeNavigateWithScrolls = mkDefault false;
          AppleEnableSwipeNavigateWithScrolls = mkDefault false;

          # Keyboard
          NSAutomaticCapitalizationEnabled = mkDefault false;
          NSAutomaticDashSubstitutionEnabled = mkDefault false;
          NSAutomaticInlinePredictionEnabled = mkDefault false;
          NSAutomaticPeriodSubstitutionEnabled = mkDefault false;
          NSAutomaticQuoteSubstitutionEnabled = mkDefault false;
          NSAutomaticSpellingCorrectionEnabled = mkDefault false;
          KeyRepeat = mkDefault 2;
          InitialKeyRepeat = mkDefault 15;

          # Scrolling
          AppleShowScrollBars = mkDefault "Automatic";
          AppleScrollerPagingBehavior = mkDefault true;

          # Finder
          AppleShowAllExtensions = mkDefault true;
          AppleShowAllFiles = mkDefault true;

          # Units
          AppleMeasurementUnits = mkDefault "Centimeters";
          AppleMetricUnits = mkDefault 1;
          AppleTemperatureUnit = mkDefault "Celsius";
        };

        alf = {
          allowdownloadsignedenabled = mkDefault 1;
          allowsignedenabled = mkDefault 1;
        };

        dock = {
          show-process-indicators = mkDefault true;
          show-recents = mkDefault true;
          tilesize = mkDefault 64;
          wvous-bl-corner = mkDefault 1;
          wvous-br-corner = mkDefault 1;
          wvous-tl-corner = mkDefault 1;
          wvous-tr-corner = mkDefault 1;
        };

        finder = {
          AppleShowAllExtensions = mkDefault true;
          AppleShowAllFiles = mkDefault true;
          CreateDesktop = mkDefault false;
          FXDefaultSearchScope = mkDefault "SCcf";
          FXEnableExtensionChangeWarning = mkDefault false;
          FXPreferredViewStyle = mkDefault "Nlsv";
          ShowStatusBar = mkDefault true;
          _FXShowPosixPathInTitle = mkDefault true;
        };

        magicmouse.MouseButtonMode = mkDefault "TwoButton";

        menuExtraClock = {
          Show24Hour = mkDefault true;
          ShowAMPM = mkDefault false;
          ShowDate = mkDefault 0;
          ShowDayOfMonth = mkDefault true;
          ShowDayOfWeek = mkDefault true;
          ShowSeconds = mkDefault false;
        };

        spaces.spans-displays = mkDefault false;

        trackpad = {
          Clicking = mkDefault false;
          Dragging = mkDefault false;
          TrackpadRightClick = mkDefault true;
        };
      };

      keyboard = {
        enableKeyMapping = mkDefault true;
        remapCapsLockToControl = mkDefault true;
      };
    };

  };
}
