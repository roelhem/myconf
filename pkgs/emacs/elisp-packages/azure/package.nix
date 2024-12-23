{ trivialBuild }:

trivialBuild {
  pname = "azure";
  version = "latest";
  src = ./.;
  packageRequires = [ ];
}
