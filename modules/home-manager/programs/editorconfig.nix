{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  configContents = lib.trivial.importTOML ../../../.editorconfig;

  cfg = config.programs.editorconfig;

  editorconfig = cfg.package;

in
{
  options.programs.editorconfig = {
    enable = mkEnableOption "{command}`editorconfig`";

    package = mkOption {
      type = types.package;
      default = pkgs.python312Packages.editorconfig;
      description = "The package containing the editorconfig executable";
    };
  };

  config = {
    home.packages = optional cfg.enable editorconfig;

    editorconfig.settings = lib.attrsets.filterAttrs (name: _: name != "root") configContents;

    programs.emacs.setq = mkIf cfg.enable { edconf-exec-path = "${editorconfig}/bin/editorconfig"; };
  };
}
