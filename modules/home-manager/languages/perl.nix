{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.perl;

  mkPerlOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  perl = cfg.package;
  pls = cfg.pls.package;
  perlnavigator = cfg.perlnavigator.package;

in
{

  options.languages.perl = {
    enable = mkEnableOption "{command}`perl`";
    package = mkOption {
      type = types.package;
      default = pkgs.perl;
    };

    pls = mkPerlOptions pkgs.pls;
    perlnavigator = mkPerlOptions pkgs.pls;
  };

  config = {
    home.packages =
      optional cfg.enable perl
      ++ optional cfg.pls.enable pls
      ++ optional cfg.perlnavigator.enable perlnavigator;

    programs.emacs.setq =
      mkIf cfg.perlnavigator.enable {
        lsp-perlnavigator-executable = "${perlnavigator}/bin/perlnavigator";
      }
      // mkIf cfg.pls.enable { lsp-pls-executable = "${pls}/bin/pls"; };
  };

}
