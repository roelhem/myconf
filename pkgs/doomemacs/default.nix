doomemacsSource:
{ lib
, stdenv
, emacs
, makeBinaryWrapper
, makeShellWrapper
, less
, git
, doomPager ? "${less} +g"
}:

let

  baseEmacs = emacs;

  mkDoomEmacs = {
    emacs ? baseEmacs,
    pager ? doomPager,
    doomDir ? "~/.doom.d",
    localDir ? "~/.doom.d/.local",
    icon ? ./icons/modern-doom3.icns,
    ...
  }: emacs.stdenv.mkDerivation {
      inherit emacs;
      doomPager = pager;
      name = "doom-" + emacs.name;
      src = doomemacsSource;
      nativeBuildInputs = [ emacs makeBinaryWrapper makeShellWrapper ];
      phases = [ "unpackPhase" "installPhase" ];

      installPhase = ''
        mkdir -p $out/doomemacs;
        mkdir -p $out/bin;

        cp -r * $out/doomemacs;

        baseEmacs="$emacs/bin/emacs";
        if [ -d "$emacs/Applications/Emacs.app" ]; then
          mkdir -p $out/Applications/DoomEmacs.app/Contents/MacOS
          cp -r $emacs/Applications/Emacs.app/Contents/Info.plist \
                $emacs/Applications/Emacs.app/Contents/PkgInfo \
                $out/Applications/DoomEmacs.app/Contents

          mkdir $out/Applications/DoomEmacs.app/Contents/Resources
          for src in $emacs/Applications/Emacs.app/Contents/Resources/*; do
            if [ "$src" != "$emacs/Applications/Emacs.app/Contents/Resources/Emacs.icns"  ]; then
              cp -r "$src" "$out/Applications/DoomEmacs.app/Contents/Resources/"
            fi
          done

          cp -f ${icon} $out/Applications/DoomEmacs.app/Contents/Resources/Emacs.icns

          makeBinaryWrapper $emacs/Applications/Emacs.app/Contents/MacOS/Emacs \
                            $out/Applications/DoomEmacs.app/Contents/MacOS/Emacs \
            --set-default EMACSDIR $out/doomemacs \
            --set-default DOOMDIR '${doomDir}' \
            --set-default DOOMLOCALDIR '${localDir}' \
            --add-flags "--init-directory $out/doomemacs"

          baseEmacs=$out/Applications/DoomEmacs.app/Contents/MacOS/Emacs
        fi

        makeBinaryWrapper $emacs/bin/emacs $out/bin/doomemacs \
          --set-default EMACSDIR $out/doomemacs \
          --set-default DOOMDIR '${doomDir}' \
          --set-default DOOMLOCALDIR '${localDir}' \
          --add-flags "--init-directory $out/doomemacs"

        makeShellWrapper $out/doomemacs/bin/doom $out/bin/doom \
          --prefix PATH ':' ${lib.makeBinPath [ git ]} \
          --set-default DOOMDIR '${doomDir}' \
          --set-default DOOMLOCALDIR '${localDir}' \
          --set-default EMACS "$baseEmacs" \
          --set-default EMACSDIR $out/doomemacs \
          --set-default DOOMPAGER "$doomPager"

        makeShellWrapper $out/doomemacs/bin/doomscript $out/bin/doomscript \
          --prefix PATH ':' ${lib.makeBinPath [ git ]} \
          --set-default DOOMDIR '${doomDir}' \
          --set-default DOOMLOCALDIR '${localDir}' \
          --set-default EMACS "$baseEmacs" \
          --set-default EMACSDIR $out/doomemacs \
          --set-default DOOMLOCALDIR '~/.doom.d/.local'
        '';
    };

in {

  doomemacs = lib.makeOverridable mkDoomEmacs {};
}
