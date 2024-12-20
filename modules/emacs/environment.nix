{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (lib) mkOption types;
in
{
  options = {
    envVariables = mkOption {
      type = with types; attrsOf str;
      default = { };
      description = '''';
    };

    envPath = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = ''
        Extra directories to add to the {env}`PATH`.
      '';
    };

    packages = mkOption {
      type = with types; listOf package;
      default = [ ];
      description = ''
        Packages that need to be available in the user environment for this emacs config.
      '';
    };
  };

  config =
    {
    };
}
