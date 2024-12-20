{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.python;

  pythonWithPackages = cfg.package.withPackages;

  extraPackages =
    pypkgs:
    let
      setuptools' = optional cfg.setuptools.enable pypkgs.setuptools;
      nose' = optional cfg.nose.enable pypkgs.nose;
      jupyter' =
        if cfg.jupyter.enable then
          [
            pypkgs.jupyter
            pypkgs.ipython
          ]
        else
          [ ];
      extra' = cfg.extraPackages pypkgs;
      numpy' =
        if cfg.numpy.enable then
          (with pypkgs; [
            numpy
            nptyping
          ])
        else
          [ ];
      matplotlib' = optional cfg.matplotlib.enable pypkgs.matplotlib;
    in
    setuptools' ++ jupyter' ++ nose' ++ extra' ++ numpy' ++ matplotlib';

  python = cfg.finalPackage;
  python-lsp-server = cfg.pylsp.package;
  black = cfg.black.package;
  pyflakes = cfg.pyflakes.package;
  isort = cfg.isort.package;
  pipenv = cfg.pipenv.package;
  pytest = cfg.pytest.package;

  ppkgs = python.pkgs;

  mkPythonPackageOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

in
{
  options.languages.python = {
    enable = mkEnableOption "{command}`python`.";

    package = mkOption {
      type = types.package;
      default = pkgs.python3;
      description = "The package of the cpython distribution to use.";
    };

    extraPackages = mkOption {
      default = self: [ ];
      type = hm.types.selectorFunction;
      defaultText = "ppkgs: []";
      example = literalExpression "ppkgs: [ ppkgs.jupyter ]";
      description = "Extra packages to install with python";
    };

    lsp = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    tree-sitter = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    conda = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    cython = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    poetry = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    pyenv = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    pylint = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    numpy = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    matplotlib = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    finalPackage = mkOption {
      type = types.package;
      visible = false;
      readOnly = true;
      description = "The final main package of python";
    };

    black = mkPythonPackageOptions ppkgs.black;
    pyflakes = mkPythonPackageOptions ppkgs.pyflakes;
    pipenv = mkPythonPackageOptions pkgs.pipenv;
    isort = mkPythonPackageOptions ppkgs.isort;
    # pynose = mkPythonPackageOptions ppkgs.pynose;
    pytest = mkPythonPackageOptions ppkgs.pytest;
    pylsp = mkPythonPackageOptions ppkgs.python-lsp-server;

    nose = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    jupyter = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

    setuptools = {
      enable = mkOption {
        type = types.bool;
        default = cfg.enable;
      };
    };

  };

  config = {
    languages.python.finalPackage = pythonWithPackages extraPackages;

    home.packages =
      optional cfg.enable python
      ++ optional cfg.pylsp.enable python-lsp-server
      ++ optional cfg.black.enable black
      ++ optional cfg.pyflakes.enable pyflakes
      ++ optional cfg.isort.enable isort
      ++ optional cfg.pipenv.enable pipenv
      ++ optional cfg.pytest.enable pytest;

    programs.matplotlib = mkIf cfg.matplotlib.enable {
      enable = mkDefault true;
    };

    programs.poetry = mkIf cfg.poetry.enable {
      enable = mkDefault true;
    };

    programs.pyenv = mkIf cfg.pyenv.enable {
      enable = mkDefault true;
    };

    programs.pylint = mkIf cfg.pylint.enable {
      enable = mkDefault true;
    };

    programs.emacs.setq =
      mkIf cfg.pylsp.enable {
        lsp-pylsp-server-command = [ "${python-lsp-server}/bin/pylsp" ];
        lsp-clients-python-command = [ "${python-lsp-server}/bin/pylsp" ];
      }
      // mkIf cfg.enable { lsp-ruff-lsp-python-path = "${python}/bin/python"; };

    programs.emacs.doomConfig.init = {
      lang.org = mkIf cfg.jupyter.enable {
        jupyter = true;
      };

      lang.python = {
        enable = cfg.enable;
        conda = cfg.conda.enable;
        cython = cfg.cython.enable;
        lsp = cfg.lsp.enable;
        poetry = cfg.poetry.enable;
        pyenv = cfg.pyenv.enable;
        # pyright = cfg.pyright.enable;
        tree-sitter = cfg.tree-sitter.enable;
      };
    };

  };
}
