{
  trivialBuild,
  lsp-mode,
}:

trivialBuild {
  pname = "lsp-bicep";
  version = "0.0.0";
  src = ./lsp-bicep.el;
  packageRequires = [ lsp-mode ];
}
