{ pkgs }: {
  deps = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hspec-discover
  ];
}
