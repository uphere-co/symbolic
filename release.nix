{ pkgs ? (import <nixpkgs>{})
, uphere-nix-overlay
}:

with pkgs;

let 
    hsconfig1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
    hsconfig2 = self: super: { 
      symbolic = self.callPackage (import ./default.nix) {};
    };
    hsconfig = self: super: (hsconfig1 self super // hsconfig2 self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.symbolic