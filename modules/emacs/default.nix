{
  lib,
  myconf-literate-config,
  name ? "default",
  ...
}:

let

  inherit (lib) mkOption types;

in

{
  imports = [
    ./initDirectory.nix
    ./environment.nix
    ./emacs.nix
    ./server.nix
    ./doom
    myconf-literate-config.results.emacs.module
  ];

  options = {
    name = mkOption {
      type = types.str;
      default = name;
      description = "The name of this emacs config.";
    };
  };
}
