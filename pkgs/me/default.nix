{
  buildDhallDirectoryPackage,
  Prelude,
  dhall-nix,
  stdenv,
}:

let

  base = buildDhallDirectoryPackage {
    name = "me";
    src = ./.;
    dependencies = [ Prelude ];
    source = true;
    document = false;
  };

  asNix = stdenv.mkDerivation {
    name = "me-dhall-to-nix";

    buildCommand = ''
      dhall-to-nix <<< "${base}/source.dhall" > $out
    '';

    buildInputs = [ dhall-nix ];
  };

  me = import "${asNix}";

in
base
// {
  inherit asNix;
  inherit (me) public;
}
