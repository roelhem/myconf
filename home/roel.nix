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
        # pgadmin4-desktopmode
        # postman
        # prusa-slicer
        # warp-terminal
        powershell
        iterm2
        # discord
        # frescobaldi
        jetbrains.phpstorm
        vscode
        vscodium
        nix-tree
        jwt-cli
      ]
    );
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
    htop.enable = true;
    editorconfig.enable = true;
    openscad.enable = true;
    openscad.lsp.enable = true;
  };

  # Install git.
  programs.git = {
    userName = "roelhem";
    userEmail = "ik@roelweb.com";
    extraConfig = {
      push = {
        autoSetupRemote = true;
      };
      init = {
        defaultBranch = "main";
      };
      github = {
        user = "roelhem";
      };
    };
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

  programs.emacs.doomConfig = {
    init = {
      enable = true;
      extraConfig = ''
        (load! "extra-init.el")
      '';
      completion = {
        company = {
          enable = false;
          childframe = true;
        };
        corfu = {
          enable = true;
          icons = true;
          orderless = true;
        };
        vertico = {
          enable = true;
          icons = true;
        };
      };
      ui = {
        deft.enable = true;
        doom.enable = true;
        doom-dashboard.enable = true;
        emoji.enable = true;
        emoji.unicode = true;
        hl-todo.enable = true;
        ligatures.enable = true;
        modeline.enable = true;
        ophints.enable = true;
        popup.enable = true;
        popup.defaults = true;
        treemacs.enable = true;
        vc-gutter.enable = true;
        vc-gutter.pretty = true;
        vi-tilde-fringe.enable = true;
        window-select.enable = true;
        window-select.numbers = true;
        window-select.switch-window = true;
      };
      editor = {
        evil = {
          enable = true;
          everywhere = true;
        };
        file-templates.enable = true;
        fold.enable = true;
        format.enable = true;
        format.onsave = true;
        multiple-cursors.enable = true;
        rotate-text.enable = true;
        snippets.enable = true;
      };
      emacs = {
        dired.enable = true;
        electric.enable = true;
        ibuffer.enable = true;
        ibuffer.icons = true;
        undo.enable = true;
        vc.enable = true;
      };
      term = {
        eshell.enable = true;
        vterm.enable = true;
      };
      checkers = {
        spell = {
          enable = false;
          enchant = true;
        };
        syntax.enable = true;
      };
      tools = {
        debugger = {
          enable = true;
          auth = true;
        };
        direnv.enable = true;
        docker.enable = true;
        editorconfig.enable = true;
        eval = {
          enable = true;
          overlay = true;
        };
        lookup.enable = true;
        lsp = {
          enable = true;
          peek = true;
        };
        magit = {
          enable = true;
          forge = true;
        };
        make.enable = true;
        prodigy.enable = true;
        tree-sitter.enable = true;
      };
      os = {
        macos.enable = pkgs.stdenv.isDarwin;
        tty.enable = true;
      };
      lang = {
        org = {
          enable = true;
          gnuplot = true;
          passwords = true;
          dragndrop = true;
          roam2 = true;
        };
        rest.enable = true;
        web = {
          enable = true;
          tree-sitter = true;
        };
        emacs-lisp.enable = true;
      };
      app = {
        everywhere.enable = false;
      };
      config = {
        default = {
          enable = true;
          bindings = true;
          smartparens = true;
        };
      };
    };
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
    python = {
      enable = true;
      matplotlib.enable = true;
      numpy.enable = true;
      jupyter.enable = true;
      lsp.enable = true;
      poetry.enable = true;
      tree-sitter.enable = true;
      extraPackages =
        pypkgs:
        (with pypkgs; [
          psycopg2
          types-psycopg2
        ]);
    };
    julia.enable = true;
    dotnet.enable = true;
    bicep.enable = false;
    nix = {
      enable = true;
      nixfmt.enable = true;
      lsp.enable = true;
      tree-sitter.enable = true;
    };
    arduino.enable = true;
    arduino.lsp.enable = true;
    agda.enable = true;
    coq.enable = true;
    haskell.enable = true;
    purescript.enable = true;
    javascript.enable = true;
    php = {
      enable = true;
      tree-sitter.enable = true;
    };
    tex = {
      enable = true;
      texlab.enable = true;
    };
    vue.enable = true;
    sh.enable = true;
    go.enable = true;
    rust = {
      enable = true;
      lsp.enable = true;
    };
    protobuf.enable = true;
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
    sql.enable = true;
    graphql.enable = true;
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
