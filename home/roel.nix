{ pkgs, ... }:

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
      nix-tree
    ]);

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
    jq.enable = true;
    neovim.enable = true;
    nix-index.enable = true;
    ripgrep.enable = true;
    zsh.enable = true;
    emacs.enable = true;
    doomemacs.enable = true;
    htop.enable = true;
  };

  # Install git.
  programs.git = {
    userName = "roelhem";
    userEmail = "ik@roelweb.com";
    includes = [];
    ignores = [
      # nix
      "/.direnv/"
      "/.devenv/"
      "/result"
      # mac
      ".DS_Store"
      ".AppleDouble"
      ".LSOverride"
      "Icon"
      "._*"
      ".DocumentRevisions-V100"
      ".fseventsd"
      ".Spotlight-V100"
      ".TemporaryItems"
      ".Trashes"
      ".VolumeIcon.icns"
      ".com.apple.timemachine.donotpresent"
      ".AppleDB"
      ".AppleDesktop"
      "Network Trash Folder"
      "Temporary Items"
      ".apdisk"
      "*.icloud"
      # linux
      "*~"
      ".fuse_hidden*"
      ".directory"
      ".Trash-*"
      ".nfs*"
      # windows
      "Thumbs.db"
      "Thumbs.db:encryptable"
      "ehthumbs.db"
      "ehthumbs_vista.db"
      "*.stackdump"
      "[Dd]esktop.ini"
      "$RECYCLE.BIN/"
      # vscode
      ".vscode/*"
      "!.vscode/settings.json"
      "!.vscode/tasks.json"
      "!.vscode/launch.json"
      "!.vscode/extensions.json"
      "!.vscode/*.code-snippets"
    ];
  };

  # Install emacs.
  # programs.emacs = {
  #   # extraPackages = epkgs: (with epkgs; [
  #   #   nix-mode
  #   #   magit
  #   # ]);
  #   # extraConfig
  # };

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

  languages = {
    nix = {
      enable = true;
      lsp.enable = true;
    };

    dotnet = {
      enable = true;
      lsp.enable = true;
    };
  };

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
