{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  ppkgs = config.languages.python.package.pkgs;
  cfg = config.languages.cmake;

  cmake = cfg.package;
  cmake-language-server = cfg.lsp.package;

in
{
  options.languages.cmake = {
    enable = mkOption {
      type = types.bool;
      default = config.languages.cc.enable;
      description = "Enable `cmake`";
    };

    package = mkOption {
      type = types.package;
      default = ppkgs.cmake;
      description = "The cmake package to use.";
    };

    lsp.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "Enable the `cmake-language-server`.";
    };

    lsp.package = mkOption {
      type = types.package;
      default = pkgs.cmake-language-server;
      description = "The `cmake-language-server` package to use.";
    };
  };

  config = {
    home.packages = optional cfg.enable cmake ++ optional cfg.lsp.enable cmake-language-server;

    programs.emacs.setq = mkIf cfg.lsp.enable {
      lsp-cmake-server-command = "${cmake-language-server}/bin/cmake-language-server";
    };
  };

}
