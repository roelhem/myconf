{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";

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
      nix-homebrew,
      emacs-overlay,
      ...
    }:
    # This is just a script with helpers to clean-up the main flake file.
    with (import ./setup {
      # Here, I should add all the overlays/modules/packages from other
      # files and the flake inputs.
      inherit inputs;

      # Add overlays from other flakes here. Custom overlays should be
      # added to the `overlays` output of the flake.
      overlays = [ emacs-overlay.overlays.default ];

      # Add the nixos modules that should be applied to all
      # nixosConfigurations that are made by the exported
      # `mkNixosConfig` helper function.
      nixosModules = [
        ./modules/shared
        ./modules/nixos
      ];

      # Add the nix-darwin modules that should be applied to all
      # darwinConfigurations that are made by the exported
      # `mkDarwinConfig` helper function.
      darwinModules = [
        nix-homebrew.darwinModules.nix-homebrew
        ./modules/shared
        ./modules/darwin
      ];
    });
    {
      # My custom helper lib. It is constructed again here so that only
      # my custom functions will be exported in this flake.
      lib = import ./lib lib;

      # Apps
      apps =
        forDarwinSystems (
          { self, ... }:
          {
            emacs-sandbox = {
              type = "app";
              program = "${self.packages.emacs-sandbox}/bin/emacs-sandbox";
            };
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

      # Packages
      packages = forAllSystems (
        { pkgs, ... }:
        {
          inherit (pkgs) doomemacs emacs-plus bicep-langserver;
          me = pkgs.dhallPackages.me;
          switch = pkgs.myconf-switch;
          myconf-literate-config = pkgs.orgTangleFile ./config.org { };
          emacs-sandbox = pkgs.emacs-sandbox;
          # doom-config =
          #  self.darwinConfigurations.home-studio.config.home-manager.users.roel.programs.emacs.doomConfig.finalDirPackage;
        }
      );

      # The custom overlays that I use. These will all be added to the global
      # overlays in the ./setup/default.nix file. I export them here mainly for
      # debugging purposes.
      overlays = {
        emacs = import ./overlays/emacs.nix inputs;
        custom-pkgs = import ./overlays/custom-pkgs.nix inputs;
        me = import ./overlays/me.nix inputs;
        tree-sitter = import ./overlays/tree-sitter.nix inputs;
        python = import ./overlays/python.nix inputs;
      };

      # ====== DevTools ======
      # Dev Shells
      devShells = forAllSystems (
        { pkgs, self, ... }: with pkgs; { doomemacs = mkShell { buildInputs = [ doomemacs ]; }; }
      );

      # Formatter
      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);

      # ====== EMACS ======
      # Emacs Configurations.
      emacsConfigurations = mkEmacsConfigs {
        default = [ { doom.enable = true; } ];
      };

      # Emacs Modules.
      emacsModules = { };

      # ====== NixOS ======
      # NixOS Configurations.
      nixosConfigurations = { };

      # NixOS Modules
      nixosModules = { };

      # ====== MacOS / Darwin ======
      # nix-darwin Configurations.
      darwinConfigurations = {
        "home-imac" = mkDarwinConfig "x86_64-darwin" [ ./hosts/darwin/home-imac ];
        "home-studio" = mkDarwinConfig "aarch64-darwin" [ ./hosts/darwin/home-studio ];
      };

      darwinModules = { };
    };
}
