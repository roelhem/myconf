{ makeElispDerivation, ... }:

{
  orgTangleFile = src:
    { langRegex ? null, targetFile ? builtins.baseNameOf src, load ? (x: [ ])
    , ... }@env:
    makeElispDerivation (env // {
      inherit targetFile langRegex src;
      name = env.name or "tangle-${targetFile}";
      buildFuncall = "builder-org-tangle-file";
      load = pkgs:
        ([ ] ++ (if builtins.isFunction load then load pkgs else load));
    });
}
