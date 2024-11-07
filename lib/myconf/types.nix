lib:

let

  inherit (lib)
    mkOption
    mkDefault
    mkIf
    types
    genList
    length
    lowerChars
    replaceStrings
    stringToCharacters
    concatStrings
    upperChars
    mapAttrsToList
    escapeShellArgs
    ;

  storeFileName =
    path:
    let
      safeChars = [
        "+"
        "."
        "_"
        "?"
        "="
      ] ++ lowerChars ++ upperChars ++ stringToCharacters "0123456789";

      empties = l: genList (x: "") (length l);

      unsafeInName = stringToCharacters (replaceStrings safeChars (empties safeChars) path);

      safeName = replaceStrings unsafeInName (empties unsafeInName) path;
    in
    safeName;
in
rec {
  files =
    {
      pkgs,
      storeNamePrefix ? "",
      copyByDefault ? false,
    }:
    types.attrsOf (
      types.submodule (
        { name, config, ... }:
        {
          options = {
            enable = mkOption {
              type = types.bool;
              default = true;
              description = "Whether to include this file in the doom config.";
            };

            target = mkOption {
              type = types.str;
              description = "Path of the file.";
            };

            text = mkOption {
              type = types.nullOr types.lines;
              default = null;
              description = "The contents of the file.";
            };

            source = mkOption {
              type = types.path;
              description = "Path of the source file or directory.";
            };

            copy = mkOption {
              type = types.bool;
              default = copyByDefault;
              description = ''
                Whether the file should be copied to the target directory
                instead of being linked into it.
              '';
            };

            executable = mkOption {
              type = types.nullOr types.bool;
              default = null;
              description = ''
                Set the execute bit. If `null`, defaults to the mode
                of the {var}`source` file or to `false` for files created
                through the {var}`text` option.
              '';
            };

            recursive = mkOption {
              type = types.bool;
              default = false;
              description = ''
                If the file source is a directory, then this option
                determines whether the directory should be recursively
                linked to the target location. This option has no effect
                if the source is a file.
              '';
            };
          };

          config = {
            target = mkDefault name;
            source = mkIf (config.text != null) (
              mkDefault (
                pkgs.writeTextFile {
                  inherit (config) text;
                  executable = config.executable == true;
                  name = storeNamePrefix + storeFileName name;
                }
              )
            );
          };
        }
      )
    );

  dir =
    {
      storeNamePrefix ? "",
      specialArgs ? { },
      shorthandOnlyDefinesConfig ? false,
      copyByDefault ? false,
      modules ? [ ],
    }:
    (types.submoduleWith {
      inherit specialArgs shorthandOnlyDefinesConfig;
      modules = [
        (
          { pkgs, config, ... }:
          {
            options = {
              name = mkOption {
                type = types.str;
                default = storeNamePrefix;
                description = "The name of the directory in the store.";
              };

              file = mkOption {
                type = files { inherit pkgs storeNamePrefix; };
                default = { };
                description = "The files of the directory.";
              };

              finalDirPackage = mkOption {
                type = types.package;
                internal = true;
                description = "Package that contains the resulting directory.";
              };
            };

            config = {
              finalDirPackage = pkgs.runCommandLocal config.name { nativeBuildInputs = [ pkgs.xorg.lndir ]; } ''
                mkdir -p $out

                # Needed in case /nix in a symbolic link.
                realOut="$(realpath -m "$out")"

                function insertFile() {
                  local source="$1"
                  local relTarget="$2"
                  local executable="$3"
                  local recursive="$4"
                  local forceCopy="$5"

                  # If the target already exists then we have a collision.
                  if [[ -e "$realOut/$relTarget" ]]; then
                    echo "File conflict for file '$relTarget'" >&2
                    exit 1
                  fi

                  # Figure out the real absolute path to the target.
                  local target
                  target="$(realpath -m "$realOut/$relTarget")"

                  # Target path must be within the output path.
                  if [[ ! $target == $realOut* ]]; then
                    echo "Error writing file '$relTarget' outside the build directory." >&2
                    exit 1
                  fi

                  mkdir -p "$(dirname "$target")"
                  if [[ -d $source ]]; then
                    if [[ $forceCopy ]]; then
                      cp -R "$source" "$target"
                    elif [[ $recursive ]]; then
                      mkdir -p "$target"
                      lndir -silent "$source" "$target"
                    else
                      ln -s "$source" "$target"
                    fi
                  else
                    [[ -x $source ]] && isExecutable=1 || isExecutable=""

                    # Link the file if the executable bit will not change.
                    if [[ ! forceCopy && ($executable = inherit || $isExecutable == $executable) ]]; then
                      ln -s "$source" "$target"
                    else
                      cp "$source" "$target"
                      if [[ $executable == inherit ]]; then
                        # Don't change file mode if it should match the source.
                        :
                      elif [[ $executable ]]; then
                        chmod +x "$target"
                      else
                        chmod -x "$target"
                      fi
                    fi
                  fi
                }

                ${concatStrings (
                  mapAttrsToList (n: v: ''
                    insertFile ${
                      escapeShellArgs [
                        (builtins.toString v.source)
                        v.target
                        (if v.executable == null then "inherit" else toString v.executable)
                        (toString v.recursive)
                        (toString v.copy)
                      ]
                    }
                  '') config.file
                )}
              '';
            };
          }
        )
      ] ++ modules;
    });
}
