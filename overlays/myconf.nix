inputs: final: prev:

let

  myconfPkgs = final.callPackage ../pkgs/myconf {
    darwin-rebuild = inputs.nix-darwin.packages.${prev.system}.darwin-rebuild;
  };

in
{
  inherit (myconfPkgs) myconf-switch;
}
