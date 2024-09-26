{ pkgs, ... }:

{

  home.username = "roel";
  home.stateVersion = "24.05";

  home = {
    packages = (
      with pkgs;
      [
        upx
        azure-cli
        azure-functions-core-tools
        graphviz
        gnuplot
        imagemagick
        pgadmin4-desktopmode
        # postman
        prusa-slicer
        # warp-terminal
        powershell
        iterm2
        teams
        # discord
        # frescobaldi
        jetbrains.phpstorm
        vscode
        vscodium
        nix-tree
      ]
    );

    file = {

    };

    sessionPath = [

    ];
  };

  programs = {
    broot.enable = true;
    btop.enable = true;
    direnv.enable = true;
    fd.enable = true;
    gh.enable = true;
    git.enable = true;
    gpg.enable = true;
    password-store.enable = true;
    home-manager.enable = true;
    neovim.enable = true;
    nix-index.enable = true;
    ripgrep.enable = true;
    zsh.enable = true;
    emacs.enable = true;
    doomemacs.enable = true;
    htop.enable = true;
    editorconfig.enable = true;
    openscad.enable = true;
    openscad.lsp.enable = true;
  };

  # Install git.
  programs.git = {
    userName = "roelhem";
    userEmail = "ik@roelweb.com";
  };

  programs.broot = {
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.nix-index = {
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  # programs.bun.enable

  # programs.chromium.enable
  # programs.firefox.enable

  # programs.darcs.enable

  # dircolors
  # eza
  # fastfetch

  languages = {
    markdown.enable = true;
    python.enable = true;
    julia.enable = true;
    dotnet.enable = true;
    nix.enable = true;
    agda.enable = true;
    coq.enable = true;
    haskell.enable = true;
    purescript.enable = true;
    typescript.enable = true;
    php.enable = true;
    tex.enable = true;
    vue.enable = true;
    sh.enable = true;
    go.enable = true;
    nginx.enable = true;
    idris2.enable = true;
    cc.enable = true;
    jq.enable = true;
    lua.enable = true;
    perl.enable = true;
    vim.enable = true;
    xml.enable = true;
    yaml.enable = true;
    toml.enable = true;
    json.enable = true;
    dhall.enable = true;
    zig.enable = true;
    java.enable = true;
    kotlin.enable = true;
    common-lisp.enable = true;
    clojure.enable = true;
    racket.enable = true;
    scheme.enable = true;
    ocaml.enable = true;
    faust.enable = true;
    dockerfile.enable = true;
    sml.enable = false;
  };

  editorconfig.enable = true;

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  targets.darwin = {
    search = "Google";
    defaults = { };
  };
}
