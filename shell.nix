with (import ./default.nix);

hsPkgs.shellFor {
  packages = _: [ aoc ];
  withHoogle = false;
  buildInputs = with pkgs; [
    cabal-install
    cabal2nix

    hsPkgs.ghcid
    hsPkgs.ormolu
    hsPkgs.hlint

    haskell-language-server
  ];
}
