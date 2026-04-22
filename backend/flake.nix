{
  description = "Backend for Catan in Haskell with Miso";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = f system;
        }) systems);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.haskell.compiler.ghc96
              pkgs.cabal-install
              pkgs.pkg-config
              pkgs.zlib
            ];

            shellHook = ''
              echo "Entered backend dev shell"
              ghc --version
              cabal --version
            '';
          };
        });
    };
}