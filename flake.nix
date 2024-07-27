{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # darwin
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # home-manager.url
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Flake compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, ... }: {

    darwinConfigurations = {
      "imac-van-roel" = nix-darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./nix/modules/nix-darwin
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users = {
                roel = ./nix/modules/home-manager
              }
            }
          }
        ];
      };
    };

  };
}
