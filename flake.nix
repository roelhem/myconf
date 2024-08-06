{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # darwin
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Home manager
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

    # Emacs
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    homebrew-emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus?dir=patches";
      flake = false;
    };

    # Flake compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, nix-darwin, home-manager
    , nix-homebrew, emacs-overlay, homebrew-emacs-plus, ... }:
    let
      lib = nixpkgs.lib;
      linuxSystems = [ "x86_64-linux" "aarch64-linux" ];
      darwinSystems = [ "x86_64-darwin" "aarch64-darwin" ];

      overlays = [
        emacs-overlay.overlays.default
        self.overlays.emacs
        self.overlays.myconf
      ];

      # System-dependent bootstrap.
      forSystem = system: f:
        f (let
          pkgs = import nixpkgs { inherit system overlays; };

          fromFlake = flake: {
            checks = flake.checks.${system};
            packages = flake.packages.${system};
            apps = flake.apps.${system};
            devShells = flake.devShells.${system};
            formatter = flake.${system};
            legacyPackages = flake.legacyPackages.${system};
          };
        in {
          inherit system pkgs;
          self = fromFlake self;
          nix-darwin = fromFlake nix-darwin;
          nixpkgs = fromFlake nixpkgs;
        });
      forSystems = xs: f: lib.genAttrs xs (system: forSystem system f);

      # System definitions.
      forLinuxSystems = forSystems linuxSystems;
      forDarwinSystems = forSystems darwinSystems;
      forAllSystems = forSystems (linuxSystems ++ darwinSystems);

    in {

      # Packages
      packages = forAllSystems ({ pkgs, nixpkgs, nix-darwin, ... }: {
        inherit (pkgs) doomemacs emacs-plus;

        example = pkgs.orgTangleFile ./config.org { };

        bootstrap = pkgs.writeShellScriptBin "bootstrap"
          "${nix-darwin.packages.darwin-rebuild}/bin/darwin-rebuild switch --flake ~/.myconf";
      });

      # Devshells
      devShells = forAllSystems ({ pkgs, self, ... }:
        with pkgs; {
          doomemacs = mkShell { buildInputs = [ doomemacs ]; };
        });

      # Apps
      apps = forDarwinSystems ({ self, ... }: {
        emacs = {
          type = "app";
          program =
            "${self.packages.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
        };

        bootstrap = {
          type = "app";
          program = "${self.packages.bootstrap}/bin/bootstrap";
        };
      }) // forLinuxSystems ({ ... }:
        {

        });

      # Overlays
      overlays = {
        emacs = import ./overlays/emacs.nix inputs;
        myconf = import ./overlays/myconf.nix inputs;
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
            nixpkgsOverlays = overlays;
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
