{ config, lib, pkgs, ... }:

{

  home.username = "roel";
  home.stateVersion = "24.05";

  home = {
    packages = (with pkgs; [
      upx
      shfmt
      azure-cli
      azure-functions-core-tools
      graphviz
      gnuplot
      imagemagick
      pgadmin4-desktopmode
      # postman
      openscad
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
    ]);

    file = {

    };

    sessionPath = [

    ];

    sessionVariables = {
      EDITOR = "emacs";
    };

    shellAliases = {

    };
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
    jq.enable = true;
    neovim.enable = true;
    nix-index.enable = true;
    ripgrep.enable = true;
    zsh.enable = true;
    emacs.enable = true;
    htop.enable = true;
  };

  # Install git.
  programs.git = {
    userName = "roelhem";
    userEmail = "ik@roelweb.com";
    includes = [];
  };

  # Install emacs.
  programs.emacs = {
    extraPackages = epkgs: (with epkgs; [
      nix-mode
      magit
    ]);
    # extraConfig
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

  programs.fd = {
    ignores = [
      ".git/"
      "*.bak"
    ];
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

  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        charset = "utf-8";
        end_of_line = "lf";
        trim_trailing_whitespace = true;
        insert_final_newline = true;
        indent_style = "space";
        indent_size = 4;
      };
    };
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  targets.darwin = {
    defaults = {

    };
  };
}
