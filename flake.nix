{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # darwin
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # home-manager.url
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # homebrew
    nix-homebrew = {
      url = "github:zhaofengli-wip/nix-homebrew";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-darwin.follows = "nix-darwin";
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };

    # Flake compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self,
                     nixpkgs,
                     flake-utils,
                     nix-darwin,
                     home-manager,
                     nix-homebrew,
                     ...
                   }:
    let
      lib = nixpkgs.lib;
      linuxSystems = [ "x86_64-linux" "aarch64-linux" ];
      darwinSystems = [ "x86_64-darwin" "aarch64-darwin" ];

      # System-dependent bootstrap.
      forSystem = system: f: f (
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = with self.overlays; [
              emacs
            ];
          };

          fromFlake = flake: {
            checks         = flake.checks.${system};
            packages       = flake.packages.${system};
            apps           = flake.apps.${system};
            devShells      = flake.devShells.${system};
            formatter      = flake.${system};
            legacyPackages = flake.legacyPackages.${system};
          };
        in
          {
            inherit system pkgs;
            self        = fromFlake self;
            nix-darwin  = fromFlake nix-darwin;
            nixpkgs     = fromFlake nixpkgs;
          });
      forSystems = xs : f : lib.genAttrs xs (system: forSystem system f);

      # System definitions.
      forLinuxSystems = forSystems linuxSystems;
      forDarwinSystems = forSystems darwinSystems;
      forAllSystems = forSystems (linuxSystems ++ darwinSystems);

    in {
      # Packages
      packages = forAllSystems ({ pkgs, nixpkgs, nix-darwin, ... }: {
        emacs = pkgs.emacs;

        bootstrap = pkgs.writeShellScriptBin "bootstrap"
          "${nix-darwin.packages.darwin-rebuild}/bin/darwin-rebuild switch --flake ~/.myconf";
      });

      # Apps
      apps = forDarwinSystems ({ self, ...}: {
        emacs = {
          type = "app";
          program = "${self.packages.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
        };

        bootstrap = {
          type = "app";
          program = "${self.packages.bootstrap}/bin/bootstrap";
        };
      }) // forLinuxSystems ({ ... }: {

      });

      # Overlays
      overlays = {
        emacs = import ./overlays/emacs.nix;
      };

      # NixOS
      nixosConfigurations = {

      };

      # MacOS
      darwinConfigurations = {
        "roel" = nix-darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = {
            inherit (inputs) homebrew-core homebrew-cask homebrew-bundle;
            overlays = self.overlays;
          };
          modules = [
            home-manager.darwinModules.home-manager
            nix-homebrew.darwinModules.nix-homebrew
            ./modules/shared
            ./modules/darwin
            ./hosts/darwin
          ];
        };
      };


    };
}
