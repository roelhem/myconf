{
  inputs,
  lib,
  ...
}:

let

  # Attrs passed to all elisp-packages
  elispPackagesAttrs = { };

in
self:
{
  unison-ts-mode = self.trivialBuild {
    pname = "unison-ts-mode";
    version = "1.0.0-rc.1";
    src = inputs.unison-ts-mode;
  };
}
// lib.packagesFromDirectoryRecursive {
  callPackage = p: a: self.callPackage p (a // elispPackagesAttrs);
  directory = ./elisp-packages;
}
