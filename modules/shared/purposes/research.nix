{
  config,
  lib,
  ...
}:

let

  inherit (lib) mkOption types mkIf;

  cfg = config.purposes.research;

in

{
  options.purposes.research = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs = {
      chromium.enable = true;
      man.enable = true;
      info.enable = true;
    };
  };
}
