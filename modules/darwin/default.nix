{
  config,
  homebrew-core,
  homebrew-cask,
  homebrew-bundle,
  defaultUser,
  ...
}:

{
  imports = [ ./programs/browsers.nix ];

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

  };
}
