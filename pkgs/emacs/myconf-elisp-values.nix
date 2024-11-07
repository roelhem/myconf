{
  lib,
  inputs,
  ...
}:

let

  debug =
    let
      value = lib.attrsets.mapAttrs (
        name:
        {
          outPath,
          rev ? null,
          shortRev ? null,
          narHash,
          lastModified ? null,
          lastModifiedDate ? null,
          ...
        }:
        {
          out-path = outPath;
          rev = rev;
          short-rev = shortRev;
          nar-hash = narHash;
          last-modified = lastModified;
          last-modified-date = lastModifiedDate;
        }
      ) inputs;
    in
    lib.elisp.toSetqWith { alist = true; } { myconf-flake-inputs = value; };

in
{

}
