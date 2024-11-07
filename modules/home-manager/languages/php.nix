{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.php;

  ppkgs = php.packages;

  php = cfg.package;
  phpactor = cfg.phpactor.package;
  composer = cfg.composer.package;
  psalm = cfg.psalm.package;
  phpstan = cfg.phpstan.package;
  phive = cfg.phive.package;
  phpinsights = cfg.phpinsights.package;
  php-cs-fixer = cfg.php-cs-fixer.package;
  phpmd = cfg.phpmd.package;
  php-codesniffer = cfg.php-codesniffer.package;
  phing = cfg.phing.package;
  # psysh = cfg.psysh.package;

  mkPhpOptions = package: {
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

  options.languages.php = {
    enable = mkEnableOption "{command}`php`";

    package = mkOption {
      type = types.package;
      default = pkgs.php;
      description = "The default php package to use.";
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

    hack = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    composer = mkPhpOptions ppkgs.composer;
    psalm = mkPhpOptions ppkgs.psalm;
    phpstan = mkPhpOptions ppkgs.phpstan;
    phive = mkPhpOptions ppkgs.phive;
    phpinsights = mkPhpOptions ppkgs.phpinsights;
    php-cs-fixer = mkPhpOptions ppkgs.php-cs-fixer;
    phpmd = mkPhpOptions ppkgs.phpmd;
    php-codesniffer = mkPhpOptions ppkgs.php-codesniffer;
    phing = mkPhpOptions ppkgs.phing;
    # psysh = mkPhpOptions ppkgs.psysh;
    phpactor = mkPhpOptions pkgs.phpactor;
  };

  config = {
    home.packages =
      optional cfg.enable php
      ++ optional cfg.composer.enable composer
      ++ optional cfg.psalm.enable psalm
      ++ optional cfg.phpstan.enable phpstan
      ++ optional cfg.phive.enable phive
      ++ optional cfg.phpinsights.enable phpinsights
      ++ optional cfg.php-cs-fixer.enable php-cs-fixer
      ++ optional cfg.phpmd.enable phpmd
      ++ optional cfg.php-codesniffer.enable php-codesniffer
      ++ optional cfg.phing.enable phing
      # ++ optional cfg.psysh.enable psysh
      ++ optional cfg.phpactor.enable phpactor;

    programs.emacs.setq = mkIf cfg.phpactor.enable { lsp-phpactor-path = "${phpactor}/bin/phpactor"; };

    programs.emacs.doomConfig.init.lang.php = {
      enable = mkDefault cfg.enable;
      hack = mkDefault cfg.hack.enable;
      lsp = mkDefault cfg.lsp.enable;
      tree-sitter = mkDefault cfg.tree-sitter.enable;
    };
  };
}
