let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz") {};
in
  rec {
    inherit pkgs;
    hsPkgs = pkgs.haskell.packages.ghc8104;
    aoc = hsPkgs.callCabal2nix "aoc-2021" ./. {};
  }
