{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # My secrets
    # mysecrets = {
    #   url = "git+ssh://git@github.com/roelhem/mysecrets";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

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
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    homebrew-emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus?dir=patches";
      flake = false;
    };

    # Emacs packages
    unison-ts-mode = {
      url = "github:fmguerreiro/unison-ts-mode";
      flake = false;
    };

    # Flake backwards compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Tree-sitter grammars
    tree-sitter-bicep = {
      url = "github:tree-sitter-grammars/tree-sitter-bicep";
      flake = false;
    };
    tree-sitter-unison = {
      url = "github:kylegoetz/tree-sitter-unison";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      nix-darwin,
      home-manager,
      nix-homebrew,
      emacs-overlay,
      homebrew-emacs-plus,
      ...
    }:
    let
      lib = nixpkgs.lib.extend (final: _: import ./lib final);

      # Supported systems.
      linuxSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      darwinSystems = [
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      overlays = [
        emacs-overlay.overlays.default
        (final: prev: { lib = prev.lib // import ./lib final.lib; })
      ] ++ builtins.attrValues self.overlays;

      # System-dependent bootstrap.
      forSystem =
        system: f:
        f (
          let
            pkgs = import nixpkgs { inherit system overlays; };

            fromFlake = flake: {
              checks = flake.checks.${system};
              packages = flake.packages.${system};
              apps = flake.apps.${system};
              devShells = flake.devShells.${system};
              formatter = flake.${system};
              legacyPackages = flake.legacyPackages.${system};
            };
          in
          {
            inherit system pkgs;
            self = fromFlake self;
            nix-darwin = fromFlake nix-darwin;
            nixpkgs = fromFlake nixpkgs;
          }
        );
      forSystems = xs: f: lib.genAttrs xs (system: forSystem system f);

      # System definitions.
      forLinuxSystems = forSystems linuxSystems;
      forDarwinSystems = forSystems darwinSystems;
      forAllSystems = forSystems (linuxSystems ++ darwinSystems);

    in
    {
      # Helper lib
      lib = import ./lib lib;

      # Formatter
      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);

      # Packages
      packages = forAllSystems (
        {
          pkgs,
          nixpkgs,
          nix-darwin,
          ...
        }:
        {
          inherit (pkgs) doomemacs emacs-plus bicep-langserver;
          me = pkgs.dhallPackages.me;
          switch = pkgs.myconf-switch;
          myconf-literate-config = pkgs.orgTangleFile ./config.org { };
          doom-config =
            self.darwinConfigurations.home-studio.config.home-manager.users.roel.programs.emacs.doomConfig.finalDirPackage;
        }
      );

      # Devshells
      devShells = forAllSystems (
        { pkgs, self, ... }: with pkgs; { doomemacs = mkShell { buildInputs = [ doomemacs ]; }; }
      );

      # Apps
      apps =
        forDarwinSystems (
          { self, ... }:
          {
            emacs = {
              type = "app";
              program = "${self.packages.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
            };

            switch = {
              type = "app";
              program = "${self.packages.switch}/bin/myconf-switch";
            };
          }
        )
        // forLinuxSystems (
          { ... }:
          {

          }
        );

      # Overlays
      overlays = {
        emacs = import ./overlays/emacs.nix inputs;
        custom-pkgs = import ./overlays/custom-pkgs.nix inputs;
        me = import ./overlays/me.nix inputs;
        tree-sitter = import ./overlays/tree-sitter.nix inputs;
        python = import ./overlays/python.nix inputs;
      };

      # NixOS
      nixosConfigurations = {

      };

      # MacOS
      darwinConfigurations =
        let

          mkDarwinConfig =
            system: modules:
            nix-darwin.lib.darwinSystem {
              inherit system;
              specialArgs = {
                inherit lib;
              };
              modules = [
                (
                  { pkgs, ... }:
                  {
                    _module.args = {
                      inherit (inputs) homebrew-core homebrew-cask homebrew-bundle;
                      inherit inputs;
                      myconf-literate-config = self.packages."${system}".myconf-literate-config;
                      nixpkgsOverlays = overlays;
                      nixformatter = self.formatter."${system}";
                      me = self.packages."${system}".me;
                      defaultUser = {
                        name = "roel";
                      };
                    };
                  }
                )
                home-manager.darwinModules.home-manager
                nix-homebrew.darwinModules.nix-homebrew
                ./modules/shared
                ./modules/darwin
              ] ++ modules;
            };

        in
        {
          "home-imac" = mkDarwinConfig "x86_64-darwin" [ ./hosts/darwin/home-imac ];
          "home-studio" = mkDarwinConfig "aarch64-darwin" [ ./hosts/darwin/home-studio ];
        };

    };
}
