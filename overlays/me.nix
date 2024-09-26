inputs: final: prev:

let

  dhallOverlay = final: prev: { me = final.callPackage ../pkgs/me { }; };

in
{
  dhallPackages = prev.dhallPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) dhallOverlay;
  });
}
