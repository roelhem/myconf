{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.languages.java;

  mkJavaOptions = package: {
    enable = mkOption {
      type = types.bool;
      default = cfg.enable;
    };

    package = mkOption {
      type = types.package;
      default = package;
    };
  };

  jdk = cfg.package;
  google-java-format = cfg.google-java-format.package;
  jdtls = cfg.jdtls.package;

in
{
  options.languages.java = {
    enable = mkEnableOption "{command} `java` language support.";

    package = mkOption {
      type = types.package;
      default = pkgs.jdk;
    };

    google-java-format = mkJavaOptions pkgs.google-java-format;
    jdtls = mkJavaOptions pkgs.jdt-language-server;

  };

  config = {
    home.packages =
      optional cfg.google-java-format.enable google-java-format
      ++ optional cfg.jdtls.enable jdtls;

    programs.java = mkIf cfg.enable {
      enable = true;
      package = jdk;
    };
  };
}