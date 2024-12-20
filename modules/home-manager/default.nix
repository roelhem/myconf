{ pkgs, ... }:

{
  imports = [
    ./programs/emacs/config.nix
    ./programs/nodejs.nix
    ./programs/git.nix
    ./programs/fd.nix
    ./programs/editorconfig.nix
    ./programs/openscad.nix
    ./programs/zsh.nix
    ./languages/arduino.nix
    ./languages/agda.nix
    ./languages/cmake.nix
    ./languages/cc.nix
    ./languages/coq.nix
    ./languages/nix.nix
    ./languages/dotnet.nix
    ./languages/bicep.nix
    ./languages/haskell.nix
    ./languages/dhall.nix
    ./languages/python.nix
    ./languages/julia.nix
    ./languages/javascript.nix
    ./languages/purescript.nix
    ./languages/vue.nix
    ./languages/nginx.nix
    ./languages/php.nix
    ./languages/perl.nix
    ./languages/go.nix
    ./languages/rust.nix
    ./languages/dockerfile.nix
    ./languages/jq.nix
    ./languages/toml.nix
    ./languages/yaml.nix
    ./languages/json.nix
    ./languages/graphql.nix
    ./languages/sql.nix
    ./languages/xml.nix
    ./languages/vim.nix
    ./languages/markdown.nix
    ./languages/org.nix
    ./languages/gleam.nix
    ./languages/idris2.nix
    ./languages/tex.nix
    ./languages/lua.nix
    ./languages/sh.nix
    ./languages/sml.nix
    ./languages/java.nix
    ./languages/kotlin.nix
    ./languages/zig.nix
    ./languages/common-lisp.nix
    ./languages/scheme.nix
    ./languages/clojure.nix
    ./languages/racket.nix
    ./languages/ocaml.nix
    ./languages/faust.nix
  ];

  config = {
    home.packages = [ pkgs.myconf-switch ];
  };
}
