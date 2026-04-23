{
  description = "Frontend for Catan in Haskell with Miso";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    miso.url = "github:haskell-miso/miso";
  };

  outputs = { self, nixpkgs, flake-utils, miso }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell.compiler.ghc96
            pkgs.cabal-install
            pkgs.pkg-config
            pkgs.zlib
          ];

          shellHook = ''
            echo "Entered normal frontend dev shell"
            ghc --version
            cabal --version
          '';
        };

        devShells.wasm = miso.devShells.${system}.wasm;
      });
}