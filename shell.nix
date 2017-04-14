{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              xml-conduit split unordered-containers vector-algorithms storable-tuple
              tagged either
              mersenne-random
              math-functions
              hblas
              lbfgs
              MemoTrie lens
              language-c containers
	      llvm-general
	      QuickCheck
	      tasty
	      tasty-golden
	      tasty-hunit
	      tasty-quickcheck
	      tasty-smallcheck
              zenc              
            ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz ];
     shellHook = ''
     '';
   }
