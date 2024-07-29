final: prev:
let

  lib = prev.lib;

in rec {
  std-emacs = prev.emacs;

  emacs-macport = prev.emacs-macport.overrideAttrs (old: {
    configureFlags = lib.lists.remove "--without-xwidgets" old.configureFlags
                     ++ ["--with-xwidgets"];
  });

  emacs = if final.stdenv.isDarwin
          then final.emacs-macport
          else final.std-emacs;
}
