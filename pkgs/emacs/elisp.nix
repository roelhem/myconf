{
  lib,
  emacs,
  runCommand,
}:

let

  toElispWith =
    {
      nullAs ? "nil",
      trueAs ? "t",
      falseAs ? "nil",
      emptyAs ? "nil",
      alist ? false,
      quoted ? false,
    }:
    val:
    let

      isElispConvertable = x: (builtins.isAttrs x) && (x ? __toElisp);

      literalToElisp =
        x:
        if isElispConvertable x then
          x.__toElisp
        else if builtins.isString x then
          x
        else
          abort "Invalid literal value. Must be a string or an elisp convertable.";

      viaToJson = x: lib.generators.toJSON { } x;

      wrapListStr =
        xs: if builtins.length xs == 0 then emptyAs else "(" + (lib.strings.concatStringsSep " " xs) + ")";

      mayQuote = x: if quoted && lib.strings.hasPrefix "(" x then "'" + x else x;

    in
    with (rec {
      alistEntry = name: value: "(${name} . ${go value})";
      plistEntry = name: value: ":${name} ${go value}";

      cases = {
        string = viaToJson;
        path = viaToJson;
        int = viaToJson;
        float = viaToJson;
        bool = x: if x then literalToElisp trueAs else literalToElisp falseAs;
        "null" = x: literalToElisp nullAs;
        list = xs: wrapListStr (map go xs);
        set = xs: wrapListStr (lib.attrsets.mapAttrsToList (if alist then alistEntry else plistEntry) xs);
        "lambda" = x: abort "Functions cannot be converted to elisp!";
      };

      go =
        x:
        if (builtins.isAttrs x && isElispConvertable x) then
          x
        else
          (if (lib.isDerivation x) then x.buildPath else cases.${builtins.typeOf x} x);
    });
    mayQuote (go val);

  toSetqWith =
    args: attrs:
    let
      entry = name: value: "${name} ${toElispWith (args // { quoted = true; }) value}";
      entries = lib.strings.concatStringsSep "\n      " (lib.attrsets.mapAttrsToList entry attrs);
    in
    "(setq ${entries})";

  # Running elisp
  defaultEmacs = emacs;

  makeElispDerivation =
    let
      fn =
        {
          name,
          emacs ? defaultEmacs,
          emacsBinaryPath ? "${emacs}/bin/emacs",
          load ? x: [ ],
          buildFuncall ? "builder-exec-and-exit",
          passThrough ? { },
          ...
        }@attrs:
        let

          loadItems = if builtins.isFunction load then load emacs.pkgs else load;

          loadItems_ = builtins.partition (x: builtins.readFileType x == "directory") loadItems;

          filesToLoad = [ ./builder.el ] ++ loadItems_.wrong;

          dirsToLoad = loadItems_.right;

          args =
            builtins.concatMap (x: [
              "-l"
              x
            ]) filesToLoad
            ++ builtins.concatMap (x: [
              "-L"
              x
            ]) dirsToLoad
            ++ [
              "-f"
              buildFuncall
            ];

        in
        (runCommand name
          {
            elispAttrs = toElispWith { alist = true; } (
              builtins.removeAttrs attrs [
                "emacs"
                "emacsBinaryPath"
                "load"
                "passThrough"
              ]
            );
          }
          ''
            ${emacsBinaryPath} --quick --batch ${builtins.toString args}
          ''
        )
        // passThrough;
    in
    lib.makeOverridable fn;

  runElispWith = attrs: elisp: makeElispDerivation (attrs // { expr = builtins.toString elisp; });

  runElisp = name: env: runElispWith (env // { inherit name; });

in
{
  inherit
    toElispWith
    toSetqWith
    makeElispDerivation
    runElispWith
    runElisp
    ;

  toElisp = toElispWith { };
  toSetq = toSetqWith { };
}
