{
  pkgs,
  nixpkgsOverlays,
  defaultUser,
  inputs,
  myconf-literate-config,
  ...
}:
{

  imports = [ ./essentials.nix ];

  home-manager = {
    sharedModules = [
      {
        _module.args = {
          inherit
            defaultUser
            inputs
            myconf-literate-config
            ;
        };
      }
      ../home-manager
    ];
  };

  nix.settings = {
    experimental-features = "nix-command flakes";
    bash-prompt-prefix = "(nix:$name)\\040";
    accept-flake-config = true;
    extra-nix-path = "nixpkgs=flake:nixpkgs";
    trusted-users = [
      "@admin"
      defaultUser.name
    ];
    substituters = [
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays = nixpkgsOverlays;
  };

  fonts.packages = with pkgs; [
    fira-code
    fira-mono
    fira-sans
    hasklig
    jetbrains-mono
    monoid
    source-code-pro
    emacs-all-the-icons-fonts
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "FiraMono"
        "Hasklig"
        "JetBrainsMono"
        "Monoid"
        "OpenDyslexic"
        "SourceCodePro"
        "NerdFontsSymbolsOnly"
      ];
    })
  ];
}
