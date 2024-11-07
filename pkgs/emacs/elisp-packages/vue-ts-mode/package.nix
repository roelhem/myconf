{
  trivialBuild,
  tstools,
}:

trivialBuild {
  pname = "vue-ts-mode";
  version = "0.0.0";
  src = ./vue-ts-mode.el;
  packageRequires = [ tstools ];
}
