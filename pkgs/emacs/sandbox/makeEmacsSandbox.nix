{
  lib,
  emacs,
  stdenv,

  name ? "default",
  configSrc ? "$HOME/.emacs.d",
  runtimeEnv ? { },
  runtimeInputs ? [ ],
}:

let

  envVariables = { } // runtimeEnv;

in

stdenv.mkDerivation {
  inherit emacs;

  name = "emacs-sandbox-${name}";

  preferLocalBuild = false;

  unpackPhase = ":";

  installPhase = ''
    mkdir -p $out/bin

    echo "$runEmacs" > $out/run.sh
    chmod +x $out/run.sh
  '';

  runEmacs = ''
    #!${stdenv.shell}
    set -e

    export PATH="${emacs}/bin:${lib.makeBinPath runtimeInputs}:$PATH"
    export EMACS_SANDBOX="${name}"

    ${lib.concatStrings (
      lib.mapAttrsToList (name: value: ''
        ${lib.toShellVar name value}
        export ${name}
      '') envVariables
    )}

    emacs "--init-directory=${configSrc}" "$@"
  '';
}
