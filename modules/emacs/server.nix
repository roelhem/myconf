{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkOption types;
in

{
  options.server = {
    name = mkOption {
      type = types.str;
      default = config.name;
    };
  };
}
