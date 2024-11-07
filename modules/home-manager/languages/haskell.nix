{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.haskell;

  ghc = cfg.ghc.package;

  hpkgs =
    let
      ghckey =
        builtins.replaceStrings
          [
            "-"
            "."
          ]
          [
            ""
            ""
          ]
          ghc.name;
    in
    pkgs.haskell.packages.${ghckey};

  cabal = cfg.cabal.package;
  stack = cfg.stack.package;
  hls = cfg.lsp.package;
  hasktags = cfg.hasktags.package;
  hlint = cfg.hlint.package;
  hoogle = cfg.hoogle.package;
  fourmolu = cfg.fourmolu.package;
  stylish-haskell = cfg.stylish-haskell.package;

  haskellTools =
    if cfg.enable then
      [
        ghc
        cabal
        stack
        hasktags
        hlint
        hoogle
        fourmolu
        hpkgs.cabal-fmt
        # (hpkgs.cabal-hoogle.overrideAttrs (old: {
        #   buildInputs = old.buildInputs ++ [cabal];
        # }))
        hpkgs.hiedb
        hpkgs.hie-bios
        hpkgs.implicit-hie
        hpkgs.hpack
        hpkgs.ormolu
        hpkgs.floskell
        hpkgs.retrie
        hpkgs.hindent
        hpkgs.c2hs
        hpkgs.c2hsc
      ]
    else
      [ ];

  haskellDebugTools =
    if cfg.enable && cfg.debug.enable then
      [
        hpkgs.ghci-dap
        hpkgs.haskell-debug-adapter
      ]
    else
      [ ];

in
{
  options.programs.ihaskell = {
    enable = mkEnableOption "{command}`ihaskell`";

    package = mkOption {
      type = types.package;
      default = hpkgs.ihaskell;
    };
  };

  options.languages.haskell = {
    enable = mkEnableOption "{command}`haskell`";

    ghc.package = mkOption {
      type = types.package;
      default = pkgs.ghc;
      description = "The default GHC version.";
    };

    ghc.packages = mkOption {
      internal = true;
      visible = false;
      type = types.attrsOf types.package;
      default = hpkgs;
      description = "The haskell packages.";
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = true;
      };
    };

    cabal = {
      package = mkOption {
        type = types.package;
        default = hpkgs.cabal-install;
        description = "The default cabal package";
      };

      config = {
        enable = mkOption {
          type = types.bool;
          default = config.languages.haskell.enable;
          description = ''
            Whether to enable managing the user-wide global configuration
            (`~/.cabal/config`) of cabal.
          '';
        };

        repositories = mkOption {
          type = types.attrsOf (
            types.submodule {
              options = {
                enable = mkOption {
                  type = types.bool;
                  default = true;
                  description = ''
                    Wheter to add this repository to the cabal config.
                  '';
                };

                url = mkOption { type = types.str; };
              };
            }
          );
        };

        nix = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Enable nix integration.

            The package (which must be locally unpacked) provides a `shell.nix`
            or `default.nix` file, this flag will cause cabal to run most commands
            through nix-shell. If both expressions are present, shell.nix is
            preferred.
          '';
        };

        tests = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Force test suites to be enabled. For most users this should not be needed,
            as we always attempt to solve for test suite dependencies, even when this
            value is `False`; furthermore, test suites are automatically enabled if
            they are requested as a built target.
          '';
        };

        coverage = mkOption {
          type = types.bool;
          default = false;
        };

        exact-configuration = mkOption {
          type = types.bool;
          default = false;
        };

        benchmarks = mkOption {
          type = types.bool;
          default = false;
        };

        http-transport = mkOption {
          type = types.enum [
            "curl"
            "wget"
            "powershell"
            "plain-http"
          ];
          default = "curl";
          description = "Set a transport to be used when making http(s) requests.";
        };

        compiler = mkOption {
          type = types.enum [
            "ghc"
            "ghcjs"
            "jhc"
            "lhc"
            "uhc"
            "haskell-suite"
          ];
          default = "ghc";
          description = ''
            Specify which compiler toolchain to be used. This is independent
            of with-compiler, because the choice of toolchain affects Cabal’s build logic.
          '';
        };

        verbose = mkOption {
          type = types.enum [
            0
            1
            2
            3
          ];
          default = 1;
          description = ''
            Control the verbosity of cabal commands, valid values are from 0 to 3.
          '';
        };

        remote-repo-cache = mkOption {
          type = types.str;
          default = ".cabal/packages";
          description = ''
            Path is relative to the home root.
          '';
        };

        world-file = mkOption {
          type = types.str;
          default = ".cabal/world";
          description = ''
            Path is relative to the home root.
          '';
        };

        extra-prog-path = mkOption {
          type = types.str;
          default = ".cabal/bin";
          description = ''
            Path is relative to the home root.
          '';
        };

        installdir = mkOption {
          type = types.str;
          default = cfg.cabal.config.extra-prog-path;
          description = ''
            Path is relative to the home root.
          '';
        };

        build-summary = mkOption {
          type = types.str;
          default = ".cabal/logs/build.log";
          description = ''
            Path is relative to the home root.
          '';
        };

        jobs = mkOption {
          type = types.either types.ints.positive (types.enum [ "$ncpus" ]);
          default = "$ncpus";
          description = ''
            Run nat jobs simultaneously when building. If `$ncpus` is
            specified, run the number of jobs equal to the number of
            CPUs. Package building is often quite parallel, so turning
            on parallelism can speed up build times quite a bit!
          '';
        };

        keep-going = mkOption {
          type = types.bool;
          default = false;
          description = ''
            If true, after a build failure, continue to build other
            unaffected packages.
          '';
        };

        constraints = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Add extra constraints to the version bounds, flag settings,
            and other properties a solver can pick for a package.
          '';
        };
        preferences = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Like constraints, but the solver will attempt to satisfy
            these preferences on a best-effort basis. The resulting
            install is locally optimal with respect to preferences;
            specifically, no single package could be replaced with
            a more preferred version that still satisfies the hard
            constraints.

            Operationally, preferences can cause the solver to attempt
            certain version choices of a package before others, which
            can improve dependency solver runtime.

            One way to use preferences is to take a known working set
            of constraints (e.g., via `cabal v2-freeze`) and record
            them as preferences. In this case, the solver will first
            attempt to use this configuration, and if this violates
            hard constraints, it will try to find the minimal number
            of upgrades to satisfy the hard constraints again.
          '';
        };

        reject-unconstrained-dependencies = mkOption {
          type = types.enum [
            "all"
            "none"
          ];
          default = "none";
          description = ''
            By default, the dependency solver can include any package
            that it’s aware of in a build plan. If you wish to restrict
            the build plan to a closed set of packages (e.g., from a
            freeze file), use this flag.

            When set to `all`, all non-local packages that aren’t goals
            must be explicitly constrained. When set to `none`, the
            solver will consider all packages.
          '';
        };

        # TODO allow-older, allow-newer, index-state, active-repositories
        # https://cabal.readthedocs.io/en/3.4/cabal-project.html?highlight=tests#cfg-field-allow-newer

        install-dirs = mkOption {
          type = types.attrsOf (
            types.submodule {
              options = {
                enable = mkOption {
                  type = types.bool;
                  default = true;
                };
                prefix = mkOption { type = types.str; };
                bindir = mkOption {
                  type = types.str;
                  default = "$prefix/bin";
                };
                libdir = mkOption {
                  type = types.str;
                  default = "$prefix/lib";
                };
                libsubdir = mkOption {
                  type = types.str;
                  default = "$abi/$libname";
                };
                dynlibdir = mkOption {
                  type = types.str;
                  default = "$libdir/$abi";
                };
                libexecdir = mkOption {
                  type = types.str;
                  default = "$prefix/libexec";
                };
                libexecsubdir = mkOption {
                  type = types.str;
                  default = "$abi/$pkgid";
                };
                datadir = mkOption {
                  type = types.str;
                  default = "$prefix/share";
                };
                datasubdir = mkOption {
                  type = types.str;
                  default = "$abi/$pkgid";
                };
                docdir = mkOption {
                  type = types.str;
                  default = "$datadir/doc/$abi/$pkgid";
                };
                htmldir = mkOption {
                  type = types.str;
                  default = "$docdir/html";
                };
                haddockdir = mkOption {
                  type = types.str;
                  default = "$htmldir";
                };
                sysconfdir = mkOption {
                  type = types.str;
                  default = "$prefix/etc";
                };
              };
            }
          );
        };
      };
    };

    stack.package = mkOption {
      type = types.package;
      default = hpkgs.stack;
    };

    hlint.package = mkOption {
      type = types.package;
      default = hpkgs.hlint;
    };

    hoogle.package = mkOption {
      type = types.package;
      default = hpkgs.hoogle;
    };

    hasktags.package = mkOption {
      type = types.package;
      default = hpkgs.hasktags;
    };

    fourmolu.package = mkOption {
      type = types.package;
      default = hpkgs.fourmolu;
    };

    stylish-haskell.package = mkOption {
      type = types.package;
      default = hpkgs.stylish-haskell;
    };

    debug.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable Haskell debug chain";
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
        description = "Enable Haskell language server debug chain";
      };

      package = mkOption {
        type = types.package;
        default = hpkgs.haskell-language-server;
      };
    };
  };

  config = {
    home.packages =
      haskellTools
      ++ haskellDebugTools
      ++ optional config.programs.ihaskell.enable config.programs.ihaskell.package
      ++ optional cfg.lsp.enable hls;

    home.file.".cabal/config" =
      let
        inherit (builtins) toString substring stringLength;
        c = cfg.cabal.config;
      in
      {
        enable = c.enable;
        text =
          let
            filterEnabled = attrsets.filterAttrs (_: { enable, ... }: enable);
            boolToStr = val: if val then "True" else "False";

            # Repositories
            repoDefs = attrsets.mapAttrsToList (
              name:
              { url, ... }:
              ''
                repository ${name}
                  url: ${url}
              ''
            ) (filterEnabled c.repositories);

            # Home path
            homePath =
              pathstr:
              let
                relPath = path.subpath.normalise pathstr;
              in
              config.home.homeDirectory + substring 1 (stringLength relPath) relPath;

            # install-dirs
            installDirs = attrsets.mapAttrsToList (
              name:
              {
                prefix,
                bindir,
                libdir,
                libsubdir,
                dynlibdir,
                libexecdir,
                libexecsubdir,
                datadir,
                datasubdir,
                docdir,
                htmldir,
                haddockdir,
                sysconfdir,
                ...
              }:
              ''
                install-dirs ${name}
                  prefix: ${prefix}
                  bindir: ${bindir}
                  libdir: ${libdir}
                  libsubdir: ${libsubdir}
                  dynlibdir: ${dynlibdir}
                  libexecdir: ${libexecdir}
                  libexecsubdir: ${libexecsubdir}
                  datadir: ${datadir}
                  datasubdir: ${datasubdir}
                  docdir: ${docdir}
                  htmldir: ${htmldir}
                  haddockdir: ${haddockdir}
                  sysconfdir: ${sysconfdir}
              ''
            ) (filterEnabled c.install-dirs);

          in
          ''
            -- -*- mode: Haskell-Cabal; -*-

            -- Generated by ~/.myconf nix-config.
            -- source file: modules/home-manager/languages/haskell.nix


            -- REPOSITORIES
            ${concatStringsSep "\n\n" repoDefs}

            -- GLOBAL CONFIG
            verbose: ${toString c.verbose}
            nix: ${boolToStr c.nix}

            compiler: ${c.compiler}
            with-compiler: ${ghc}/bin/ghc

            jobs: ${if c.jobs == "$ncpus" then "$ncpus" else toString c.jobs}
            tests: ${boolToStr c.tests}
            coverage: ${boolToStr c.coverage}
            exact-configuration: ${boolToStr c.exact-configuration}
            benchmarks: ${boolToStr c.benchmarks}

            remote-repo-cache: ${homePath c.remote-repo-cache}
            world-file: ${homePath c.world-file}
            extra-prog-path: ${homePath c.extra-prog-path}
            build-summary: ${homePath c.build-summary}
            installdir: ${homePath c.installdir}

            http-transport: ${c.http-transport}

            constraints:
                ${concatStringsSep "\n  , " c.constraints}
            preferences:
                ${concatStringsSep "\n  , " c.preferences}
            reject-unconstrained-dependencies: ${c.reject-unconstrained-dependencies}

            -- INSTALL DIRS
            ${concatStringsSep "\n\n" installDirs}
          '';
      };

    languages.haskell.cabal.config = {
      repositories = {
        "hackage.hackage.org" = {
          url = "http://hackage.haskell.org/";
        };
      };

      install-dirs = {
        user = {
          prefix = "${config.home.homeDirectory}/.cabal";
        };
        global = {
          prefix = "/usr/local";
        };
      };
    };

    programs.emacs.setq = mkIf cfg.enable {
      haskell-process-path-ghci = "${ghc}/bin/ghci";
      flycheck-haskell-ghc-executable = "${ghc}/bin/ghc";
      haskell-compile-command = "${ghc}/bin/ghc -Wall -ferror-spans -fforce-recomp -c %s";
      haskell-hasktags-path = "${hasktags}/bin/hasktags";
      haskell-hoogle-command = "${hoogle}/bin/hoogle --count=40";
      haskell-process-path-cabal = "${cabal}/bin/cabal";
      haskell-compile-cabal-build-command = "${cabal}/bin/cabal build --ghc-options=-ferror-spans";
      haskell-compile-cabal-build-alt-command = "${cabal}/bin/cabal clean -s && ${cabal}/bin/cabal build --ghc-options=-ferror-spans";
      haskell-process-path-stack = "${stack}/bin/stack";
      haskell-compile-stack-build-command = "${stack}/bin/stack build --fast";
      haskell-compile-stack-build-alt-command = "${stack}/bin/stack clean && ${stack}/bin/stack build --fast";
      flycheck-haskell-stack-ghc-executable = "${stack}/bin/stack";
      lsp-haskell-server-path = "${hls}/bin/haskell-language-server-wrapper";
      lsp-haskell-formatting-provider = "${fourmolu}/bin/fourmolu";
      haskell-mode-stylish-haskell-path = "${stylish-haskell}/bin/stylish-haskell";
      haskell-check-command = "${hlint}/bin/hlint";
      flycheck-haskell-hlint-executable = "${hlint}/bin/hlint";
    };

    programs.emacs.doomConfig.init.lang.haskell = {
      enable = cfg.enable;
      lsp = cfg.lsp.enable;
      tree-sitter = cfg.tree-sitter.enable;
    };
  };
}
