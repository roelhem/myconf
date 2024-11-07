lib:
with lib;

rec {
  toElispWith =
    {
      nullAs ? "nil",
      trueAs ? "t",
      falseAs ? "nil",
      emptyAs ? "nil",
      alist ? false,
      quoted ? false,
      ...
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

      viaToJson = x: generators.toJSON { } x;

      wrapListStr =
        xs: if builtins.length xs == 0 then emptyAs else "(" + (strings.concatStringsSep " " xs) + ")";

      mayQuote = x: if quoted && strings.hasPrefix "(" x then "'" + x else x;

    in
    with (rec {
      alistEntry =
        name: value:
        if builtins.isList value then
          (
            if builtins.length value == 0 then
              "(${name})"
            else
              "(${name} ${strings.concatStringsSep " " (map go xs)})"
          )
        else if builtins.isAttrs value then
          (
            if builtins.length (builtins.attrNames value) == 0 then
              "(${name})"
            else
              "(${name} ${strings.concatStringsSep " " (attrsets.mapAttrsToList alistEntry value)})"
          )
        else if builtins.isNull value then
          "(${name})"
        else
          "(${name} . ${go value})";
      plistEntry = name: value: ":${name} ${go value}";

      cases = {
        string = viaToJson;
        path = viaToJson;
        int = viaToJson;
        float = viaToJson;
        bool = x: if x then literalToElisp trueAs else literalToElisp falseAs;
        "null" = x: literalToElisp nullAs;
        list = xs: wrapListStr (map go xs);
        set = xs: wrapListStr (attrsets.mapAttrsToList (if alist then alistEntry else plistEntry) xs);
        "lambda" = x: abort "Functions cannot be converted to elisp!";
      };

      go =
        x:
        if (builtins.isAttrs x && isElispConvertable x) then
          x
        else
          (if (isDerivation x) then x.buildPath else cases.${builtins.typeOf x} x);
    });
    mayQuote (go val);

  toElisp = toElispWith { };

  toSetqWith =
    args: attrs:
    let
      entry = name: value: "${name} ${toElispWith (args // { quoted = true; }) value}";
      entries = strings.concatStringsSep "\n      " (attrsets.mapAttrsToList entry attrs);
    in
    "(setq ${entries})";

  toSetq = toSetqWith { };

  toDefconstWith = args: symbol: value: docstring: ''
    (defconst ${symbol}
     ${toElispWith (args // { quoted = true; }) value}
     "${docstring}")
  '';
}
