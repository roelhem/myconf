inputs: final: prev:

let

  packageExtension = pfinal: pprev: {
    fabric = pprev.fabric.overridePythonAttrs (
      {
        dependencies ? [ ],
        ...
      }:
      {
        dependencies = [ pfinal.pynacl ] ++ dependencies;
      }
    );
  };

in
{
  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [ packageExtension ];
}
