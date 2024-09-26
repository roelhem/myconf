{
  writeScriptBin,
  darwin-rebuild,
  myconf-dir ? "~/.myconf",
}:

{
  myconf-switch = writeScriptBin "myconf-switch" ''
    ${darwin-rebuild}/bin/darwin-rebuild switch --flake ${myconf-dir}
  '';
}
