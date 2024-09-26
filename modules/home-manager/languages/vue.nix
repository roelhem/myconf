{ config, lib, ... }:

with lib;

let

  cfg = config.languages.vue;

  npkgs = config.programs.nodejs.package.pkgs;

  vue-language-server = cfg.vuels.package;
  volar-language-server = cfg.volarls.package;

in
{

  options.languages.vue = {
    enable = mkEnableOption "{command} VueJS";

    vuels.enable = mkOption {
      type = types.bool;
      default = cfg.enable;
      description = "{command}`vue-language-server`";
    };

    vuels.package = mkOption {
      type = types.package;
      default = npkgs."@vue/language-server";
    };

    volarls.enable = mkEnableOption "{command}`volar-language-server`";
    volarls.package = mkOption {
      type = types.package;
      default = npkgs."@volar/vue-language-server";
    };
  };

  config = {
    home.packages =
      optional cfg.vuels.enable vue-language-server
      ++ optional cfg.volarls.enable volar-language-server;
  };

}
