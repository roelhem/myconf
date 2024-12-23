{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib)
    mkEnableOption
    mkOption
    optional
    types
    ;

  inherit (lib.attrsets) filterAttrsRecursive;

  cfg = config.programs.arduino-cli;

  homeDirectory = config.home.homeDirectory;

  defaultDataPath = if pkgs.stdenv.isDarwin then "Library/Arduino15" else ".arduino15";

in

{
  options.programs.arduino-cli = {
    enable = mkEnableOption "{command}`arduino-cli`";

    package = mkOption {
      type = types.package;
      default = pkgs.arduino-cli;
    };

    configurationFile = mkOption {
      type = types.path;
      default = "${homeDirectory}/${defaultDataPath}/arduino-cli.yaml";
    };

    configuration = mkOption {
      default = { };
      type = types.submodule (
        { config, ... }:
        {
          options = {
            board_manager = {
              additional_urls = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "the URLs to any additional Boards Manager package index files needed for your boards platforms.";
              };
            };
            daemon = {
              port = mkOption {
                type = types.str;
                default = "50051";
                description = "TCP port used for gRPC client connections.";
              };
            };
            directories = {
              data = mkOption {
                type = types.nullOr types.path;
                description = "directory used to store Boards/Library Manager index files and Boards Manager platform installations.";
                default = "${homeDirectory}/${defaultDataPath}";
              };
              downloads = mkOption {
                type = types.nullOr types.path;
                description = "directory used to stage downloaded archives during Boards/Library Manager installations.";
                default = "${config.directories.data}/staging";
              };
              user = mkOption {
                type = types.nullOr types.path;
                description = " the equivalent of the Arduino IDE's 'sketchbook' directory. Library Manager installations are made to the libraries subdirectory of the user directory. Users can manually install 3rd party platforms in the hardware subdirectory of the user directory.";
                default =
                  if pkgs.stdenv.isDarwin then "${homeDirectory}/Documents/Arduino" else "${homeDirectory}/Arduino";
              };
              builtin.libraries = mkOption {
                type = types.nullOr types.path;
                description = "the libraries in this directory will be available to all platforms without the need for the user to install them, but with the lowest priority over other installed libraries with the same name, it's the equivalent of the Arduino IDE's bundled libraries directory.";
                default = null;
              };
            };
            library = {
              enable_unsafe_install = mkOption {
                type = types.bool;
                default = false;
                description = "set to true to enable the use of the `--git-url` and `--zip-file` flags with `arduino-cli lib install`. These are considered 'unsafe' installation methods because they allow installing files that have not passed through the Library Manager submission process.";
              };
            };
            locale = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The language used by Arduino CLI to communicate to the user, the parameter is the language identifier in the standard POSIX format `<language>[_<TERRITORY>[.<encoding>]]` (for example `it` or `it_IT`, or `it_IT.UTF-8`).";
            };
            logging = {
              file = mkOption {
                type = types.nullOr types.path;
                default = null;
                description = "Path to the file where logs will be written.";
              };
              format = mkOption {
                type = types.enum [
                  "text"
                  "json"
                ];
                default = "text";
                description = "output format for the logs. Allowed values are `text` or `json`.";
              };
              level = mkOption {
                type = types.enum [
                  "trace"
                  "debug"
                  "info"
                  "warn"
                  "error"
                  "fatal"
                  "panic"
                ];
                default = "info";
                description = "messages with this level and above will be logged.";
              };
            };
            metrics = {
              enabled = mkOption {
                type = types.bool;
                default = false;
                description = "Enables use of metrics.";
              };
            };
            output = {
              no_color = mkOption {
                type = types.bool;
                default = false;
                description = "ANSI color escape codes are added by default to the output. Set to `true` to disable colored text output.";
              };
            };
            sketch = {
              always_export_binaries = mkOption {
                type = types.bool;
                default = false;
                description = "Set to `true` to make `arduino-cli compile` always save binaries to the sketch folder. This is the equivalent of using the `--export-binaries` flag.";
              };
            };
            updater = {
              enable_notification = mkOption {
                type = types.bool;
                default = false;
                description = "set to `true` to enable notifications of new Arduino CLI releases.";
              };
            };
            build_cache = {
              path = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "The path to the build cache.";
              };
              extra_paths = mkOption {
                type = types.listOf types.path;
                default = [ ];
                description = "A list of paths to look for precompiled artifacts if not found on `build_cache.path` setting.";
              };
              compilations_before_purge = mkOption {
                type = types.int;
                default = 10;
                description = "Interval, in number of compilations, at which the cache is purged, defaults to 10. When 0 the cache is never purged.";
              };
              ttl = mkOption {
                type = types.str;
                default = "720h";
                description = "cache expiration time of build folders. If the cache is hit by a compilation the corresponding build files lifetime is renewed. The value format must be a valid input for `time.ParseDuration()` from go.";
              };
            };
            network = {
              proxy = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "URL of the proxy server";
              };
            };
          };
        }
      );
    };
  };

  config = {
    home.packages = optional cfg.enable cfg.package;

    home.file."${cfg.configurationFile}" = {
      enable = cfg.enable;
      text = builtins.toJSON (filterAttrsRecursive (_: val: val != null) cfg.configuration);
      onChange = "${cfg.package}/bin/arduino-cli core update-index";
    };
  };
}
