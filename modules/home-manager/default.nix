{ pkgs, ... }:

{
  imports = [
    ./emacs/config.nix
    ./emacs/doom.nix
    ./languages/nix.nix
    ./languages/dotnet.nix
  ];

  config = {
    home.packages = [
      pkgs.myconf-switch
    ];
  };
}
