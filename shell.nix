{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  myPkg = pkgs.haskellPackages.callCabal2nix "aoc24" src {};
in
pkgs.stdenv.mkDerivation {
  name = "aoc-shell";

  buildInputs = [
    myPkg.env.nativeBuildInputs

    pkgs.haskell-language-server
    pkgs.cabal-install
    pkgs.hlint
  ];
}
