inputs: final: prev:

let

  mkPackage = def: final.callPackage (final.lib.makeOverridable def);

  myconfPkgs = final.callPackage ../pkgs/myconf {
    darwin-rebuild = inputs.nix-darwin.packages.${prev.system}.darwin-rebuild;
  };

in
{
  inherit (myconfPkgs) myconf-switch;

  bicep-langserver = mkPackage (import ../pkgs/bicep-langserver) { };
}
