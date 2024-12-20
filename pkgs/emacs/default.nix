{
  lib,
  emacs,
  runCommand,
  callPackage,
  ...
}@pkgs:

let
  defaultEmacs = emacs;

in
rec {
  runElispWith = attrs: elisp: makeElispDerivation (attrs // { expr = builtins.toString elisp; });

  runElisp = name: env: runElispWith (env // { inherit name; });

  makeElispDerivation =
    let
      fn =
        {
          name,
          emacs ? defaultEmacs,
          emacsBinaryPath ? "${emacs}/bin/emacs",
          load ? x: [ ],
          buildFuncall ? "builder-exec-and-exit",
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
            elispAttrs = lib.elisp.toElispWith { alist = true; } (
              builtins.removeAttrs attrs [
                "emacs"
                "emacsBinaryPath"
                "load"
              ]
            );
          }
          ''
            ${emacsBinaryPath} --quick --batch ${builtins.toString args}
          ''
        );
    in
    lib.makeOverridable fn;

  orgTangleFile =
    src:
    {
      langRegex ? null,
      targetFile ? builtins.baseNameOf src,
      load ? (x: [ ]),
      ...
    }@env:
    let
      drv = makeElispDerivation (
        env
        // {
          inherit targetFile langRegex src;
          name = env.name or "tangle-${targetFile}";
          buildFuncall = "builder-org-tangle-file";
          load = pkgs: ([ ] ++ (if builtins.isFunction load then load pkgs else load));
        }
      );

      resultArgs = pkgs // builtins.removeAttrs env [ "load" ];

      results =
        let
          inherit (builtins) pathExists isFunction;
          resultsFile = "${drv}/results.nix";
          value = import resultsFile;
        in
        if pathExists resultsFile then
          (if isFunction value then callPackage value resultArgs else value)
        else
          null;
    in
    drv // { inherit results; };

  emacs-launch-editor = callPackage ./launch-editor.nix { };
  emacs-sandbox = callPackage ./sandbox { };
}
