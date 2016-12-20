with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc7103;
in
  haskell.lib.buildStackProject {
     name = "compiler";
     ghc = hsPkgs.ghc;
     buildInputs = [ git cabal-install stack
                     llvm_35 ];
     LANG = "en_US.UTF-8";
  }
