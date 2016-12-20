with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "compiler";
     ghc = hsPkgs.ghc;
     buildInputs = [ git cabal-install stack ];
     LANG = "en_US.UTF-8";
  }
