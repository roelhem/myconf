let
  flake = builtins.getFlake (toString ./.);
  nixpkgs = import <nixpkgs> { };
in
{
  inherit flake;
  pkgs = nixpkgs;
}
