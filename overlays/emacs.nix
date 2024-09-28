inputs: final: prev:
let

  elisp = final.callPackage ../pkgs/emacs/elisp.nix { };

  org-tangle = final.callPackage ../pkgs/emacs/org-tangle.nix { };

  emacs-zsh-plugin =
    final.callPackage (final.lib.makeOverridable (import ../pkgs/emacs-zsh-plugin))
      { };

  emacs-launch-editor =
    final.callPackage (final.lib.makeOverridable (import ../pkgs/emacs/launch-editor.nix))
      { };

  lib = prev.lib // {
    elisp = {
      inherit (elisp)
        toElisp
        toElispWith
        toSetq
        toSetqWith
        ;
    };
  };

  forceDarwinXWidgets =
    drv:
    drv.overrideAttrs (
      old:
      if drv.stdenv.isDarwin then
        {
          configureFlags = lib.lists.remove "--without-xwidgets" old.configureFlags ++ [ "--with-xwidgets" ];

          buildInputs =
            old.buildInputs
            ++ (with final.darwin.apple_sdk.frameworks; [
              QuartzCore
              WebKit
            ]);
        }
      else
        { }
    );

  # Replace map icon.
  replaceAppIconWith =
    icns: drv:
    drv.overrideAttrs (old: {
      postInstall =
        old.postInstall
        + ''

          if [ -f $out/Applications/Emacs.app/Contents/Resources/Emacs.icns ]; then
             cp -f ${icns} $out/Applications/Emacs.app/Contents/Resources/Emacs.icns
          fi

        '';
    });

  emacsPlusIcon = icns: inputs.homebrew-emacs-plus.outPath + "/icons/" + icns + ".icns";

  # Emacs plus patches.
  emacsPlusPatches =
    # with builtins;
    let
      patchesDirPath = inputs.homebrew-emacs-plus.outPath + "/patches";

      versionToDirName =
        version:
        let
          majorVersion = lib.versions.major version;
        in
        "emacs-" + majorVersion;

      isSupportedVersion =
        version:
        builtins.elem (versionToDirName version) (builtins.attrNames (builtins.readDir patchesDirPath));

      dirPathForVersion = version: patchesDirPath + "/" + (versionToDirName version);

      allPatchNamesForVersion =
        version: builtins.attrNames (builtins.readDir (dirPathForVersion version));

      patchForVersion =
        version: patchName:
        let
          patchFilePath = (dirPathForVersion version) + "/" + patchName;
        in
        final.writeText patchName (builtins.readFile patchFilePath);

      _patchesForVersion = version: map (patchForVersion version);

      patchesForVersion =
        version: patchNames:
        let
          allPatchNames = allPatchNamesForVersion version;
          existingPatchNames = builtins.filter (x: builtins.elem x allPatchNames) patchNames;
        in
        _patchesForVersion version existingPatchNames;

      allPatchesForVersion = version: _patchesForVersion version (allPatchNamesForVersion version);

      coEmacsToVersion =
        f: empty: emacs:
        if emacs.stdenv.isDarwin && isSupportedVersion emacs.version then f emacs.version else empty;

    in
    {
      inherit patchesForVersion allPatchNamesForVersion allPatchesForVersion;

      forVersion = patchesForVersion;

      allNamesFor = coEmacsToVersion allPatchNamesForVersion [ ];
      for = coEmacsToVersion patchesForVersion (x: [ ]);
      allFor = coEmacsToVersion allPatchesForVersion [ ];
    };

  mkEmacsPlus =
    baseEmacs:
    args@{ ... }:
    builtins.foldl' (drv: fn: fn drv) baseEmacs [
      (
        drv:
        {
          withX = true;
          withGTK3 = true;
        }
        // drv.override args
      )
      forceDarwinXWidgets
      (replaceAppIconWith (emacsPlusIcon "memeplex-slim"))
      (
        drv:
        drv.overrideAttrs (old: {
          name = "emacs-plus-" + drv.version;

          patches =
            (old.patches or [ ])
            ++ emacsPlusPatches.for drv (args.plusPatches or emacsPlusPatches.allNamesFor baseEmacs);
        })
      )
    ];

  emacs-plus = lib.makeOverridable mkEmacsPlus final.emacs { };

  # emacs-macport = prev.emacs-macport.overrideAttrs (old: {
  #   configureFlags = lib.lists.remove "--without-xwidgets" old.configureFlags ++ ["--with-xwidgets"];
  # });

in
{

  inherit
    lib
    emacs-plus
    emacs-zsh-plugin
    emacs-launch-editor
    ;

  inherit (elisp) makeElispDerivation runElispWith runElisp;

  inherit (org-tangle) orgTangleFile;

  inherit (final.callPackage (import ../pkgs/doomemacs inputs.doomemacs) { emacs = emacs-plus; })
    doomemacs
    ;
}
