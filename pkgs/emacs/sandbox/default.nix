{ callPackage, stdenv }:

let

  makeEmacsSandbox = callPackage ./makeEmacsSandbox.nix;

in

stdenv.mkDerivation {
  name = "emacs-sandbox";
  preferLocalBuild = true;

  unpackPhase = ":";

  installPhase = ''
    mkdir -p $out/bin
    echo "$shellHook" > $out/bin/emacs-sandbox
    chmod +x $out/bin/emacs-sandbox
  '';

  shellHook = ''
    #!${stdenv.shell}
    set -e

    config_name="''${1:-default}"

    MYCONF_HOME="''${MYCONF_HOME:-"$HOME/.myconf"}"
    cd $MYCONF_HOME;

    drv="$(nix build .#emacsConfigurations.${stdenv.system}.$config_name --print-out-paths)"
    $drv/run.sh
  '';

  passthru = {
    inherit makeEmacsSandbox;
  };
}
