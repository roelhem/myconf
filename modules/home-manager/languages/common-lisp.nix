{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.common-lisp;

  mkClOptions = enable: package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable && enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  sbcl = cfg.sbcl.package;
  ccl = cfg.ccl.package;
  clasp = cfg.clasp.package;
  abcl = cfg.abcl.package;

in
{

  options.languages.common-lisp = {
    enable = mkEnableOption "{command} `common-lisp` language support.";

    sbcl = mkClOptions true pkgs.sbcl;
    ccl = mkClOptions false pkgs.ccl;
    clasp = mkClOptions false pkgs.clasp-common-lisp;

    abcl = mkClOptions config.languages.java.enable pkgs.abcl;
  };

  config = {

    home.packages =
      optional cfg.sbcl.enable sbcl
      ++ optional cfg.ccl.enable ccl
      ++ optional cfg.clasp.enable clasp
      ++ optional cfg.abcl.enable abcl;

  };

}
