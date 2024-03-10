final: prev: {
  # Ref:
  # https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/tools/parsing/tree-sitter/grammars
  tree-sitter = prev.tree-sitter.override {
    extraGrammars = {
      tree-sitter-nim = final.lib.importJSON ./tree-sitter-grammars/nim.json;
    };
  };
}
