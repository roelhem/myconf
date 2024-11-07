inputs: final: prev:

{
  tree-sitter = prev.tree-sitter.override (old: {
    extraGrammars = {
      tree-sitter-bicep = {
        src = inputs.tree-sitter-bicep;
      };
      tree-sitter-unison = {
        src = inputs.tree-sitter-unison;
      };
    };
  });
}
