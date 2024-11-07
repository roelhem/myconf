lib:

let
  inherit (lib) types mkOption;
in

{
  # Constructs the type for an elisp file.
  #
  # Arguments:
  # - opt           the name of the option, for self reference.
  # - basePath      The file base path.
  elispFileType =
    opt: basePath:
    types.submodule (
      { config, ... }:
      {
        options = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Whether this file should be generated.
            '';
          };

          text = mkOption {
            default = null;
            type = types.nullOr types.lines;
            description = ''
              Text of the elisp file.
            '';
          };
        };
      }
    );
}
