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
    in
    setuptools' ++ jupyter' ++ nose' ++ extra';

  python = cfg.finalPackage;
  python-lsp-server = cfg.pylsp.package;
  black = cfg.black.package;
  pyflakes = cfg.pyflakes.package;
  isort = cfg.isort.package;
  pipenv = cfg.pipenv.package;
  # pynose = cfg.pynose.package;
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
    nose.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };
    jupyter.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };
    setuptools.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

  };

  config = {
    home.packages =
      optional cfg.enable python
      ++ optional cfg.pylsp.enable python-lsp-server
      ++ optional cfg.black.enable black
      ++ optional cfg.pyflakes.enable pyflakes
      ++ optional cfg.isort.enable isort
      ++ optional cfg.pipenv.enable pipenv
      # ++ optional cfg.pynose.enable pynose
      ++ optional cfg.pytest.enable pytest;

    programs.poetry = mkIf cfg.enable { enable = true; };

    programs.emacs.setq =
      mkIf cfg.pylsp.enable {
        lsp-pylsp-server-command = [ "${python-lsp-server}/bin/pylsp" ];
        lsp-clients-python-command = [ "${python-lsp-server}/bin/pylsp" ];
      }
      // mkIf cfg.enable { lsp-ruff-lsp-python-path = "${python}/bin/python"; };

    languages.python.finalPackage = pythonWithPackages extraPackages;
  };
}