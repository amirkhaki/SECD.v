{
  description = "Devshell for Haskell/Cabal project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages; [
            ghc
            cabal-install
            ghcid
          ] ++ (with pkgs; [
            rocq-core
            rocqPackages.stdlib
          ]);
        };
      }
    );
}
