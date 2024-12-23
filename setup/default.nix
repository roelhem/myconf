{
  inputs,
  linuxSystems ? [
    "x86_64-linux"
    "aarch64-linux"
  ],
  darwinSystems ? [
    "x86_64-darwin"
    "aarch64-darwin"
  ],
  libExtensions ? [ ],
  overlays ? [ ],
  darwinModules ? [ ],
  nixosModules ? [ ],
}:

let
  inherit (inputs)
    nixpkgs
    nixpkgs-stable
    home-manager
    nix-darwin
    nix-homebrew
    ;

  # Extensions for the standard library.
  libExtensions' = nixpkgs.lib.fixedPoints.composeManyExtensions (
    [
      (final: prev: { inherit (home-manager.lib) hm; })
      (final: prev: import ../lib final)
    ]
    ++ libExtensions
  );

  # Quick access to the fully extended library.
  lib = nixpkgs.lib.extend libExtensions';

  # Package overlays.
  overlays' =
    overlays
    ++ [
      (final: prev: { lib = lib.extend libExtensions'; })
    ]
    ++ builtins.attrValues inputs.self.overlays;

  # System-dependent bootstrap.
  forSystem =
    system: f:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = overlays';
      };

      pkgs-stable = import nixpkgs-stable {
        inherit system;
        overlays = overlays';
      };

      fromFlake = flake: {
        checks = flake.checks.${system};
        packages = flake.packages.${system};
        apps = flake.apps.${system};
        devShells = flake.devShells.${system};
        legacyPackages = flake.legacyPackages.${system};
        formatter = flake.formatter.${system};
      };
    in
    f {
      inherit system pkgs pkgs-stable;
      myconf-literate-config = inputs.self.packages.${system}.myconf-literate-config;
      self = fromFlake inputs.self;
      nixpkgs = fromFlake nixpkgs;
      nixpkgs-stable = fromFlake nixpkgs-stable;
      nix-darwin = fromFlake nix-darwin;
      nix-homebrew = fromFlake nix-homebrew;
      home-manager = fromFlake home-manager;
    };

  forEachSystem = xs: f: lib.genAttrs xs (system: forSystem system f);

  allSystems = (linuxSystems ++ darwinSystems);

  # Creating emacs configurations
  mkEmacsConfig =
    system: name: modules:
    import ./eval-emacs-config.nix (
      forSystem system (
        { pkgs, myconf-literate-config, ... }:
        {
          inherit
            pkgs
            lib
            inputs
            modules
            ;

          specialArgs = {
            inherit myconf-literate-config name;
          };
        }
      )
    );

in

{
  inherit
    lib
    linuxSystems
    darwinSystems
    allSystems
    forSystem
    forEachSystem
    mkEmacsConfig
    ;

  # The final list of lib extensions
  libExtensions = libExtensions';
  overlays = overlays';

  # Convenience helpers
  forLinuxSystems = forEachSystem linuxSystems;
  forDarwinSystems = forEachSystem darwinSystems;
  forAllSystems = forEachSystem allSystems;

  # nixos configuration helpers
  mkNixosConfig =
    system: modules:
    nixpkgs.lib.nixosSystem (
      forSystem system (
        { self, pkgs-stable, ... }:
        {
          inherit system;
          specialArgs = {
            inherit inputs;
            inherit (self.packages) myconf-literate-config me;
            defaultUser = {
              name = "roel";
            };
          };
          modules =
            [
              {
                _module.args = {
                  nixformatter = self.formatter;
                  inherit pkgs-stable;
                };
                nixpkgs.overlays = overlays';
              }
              home-manager.nixosModules.home-manager
            ]
            ++ nixosModules
            ++ modules;
        }
      )
    );

  # nix-darwin configuration helpers
  mkDarwinConfig =
    system: modules:
    nix-darwin.lib.darwinSystem (
      forSystem system (
        { self, pkgs-stable, ... }:
        {
          inherit system;
          specialArgs = {
            inherit lib inputs;
            inherit (inputs) homebrew-core homebrew-cask homebrew-bundle;
            inherit (self.packages) myconf-literate-config me;
            defaultUser = {
              name = "roel";
            };
          };
          modules =
            [
              {
                _module.args = {
                  nixformatter = self.formatter;
                  inherit pkgs-stable;
                };
                nixpkgs.overlays = overlays';
              }
              home-manager.darwinModules.home-manager
            ]
            ++ darwinModules
            ++ modules;
        }
      )
    );

  # Convenience helpers
  mkEmacsConfigs =
    configs: lib.genAttrs allSystems (system: (builtins.mapAttrs (mkEmacsConfig system) configs));
}
