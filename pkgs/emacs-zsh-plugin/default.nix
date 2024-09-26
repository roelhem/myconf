{ stdenv }:

let
  name = "emacs-zsh-plugin";

  drv = stdenv.mkDerivation ({
    inherit name;

    src = ./src;

    strictDeps = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p $out/share/${name}
      cp *.zsh $out/share/${name}/
    '';

    passthru = {
      activationScript = ''
        if [[ -f "${drv}/share/${name}/emacs.plugin.zsh" ]]; then
          source "${drv}/share/${name}/emacs.plugin.zsh"
        fi
      '';
    };
  });
in
drv
