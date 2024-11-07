{ trivialBuild, persp-mode }:

trivialBuild {
  pname = "myconf";
  version = "0.0.0";
  src = ./.;
  packageRequires = [ persp-mode ];
}
