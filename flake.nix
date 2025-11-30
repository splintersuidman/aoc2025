{
  description = "Flake for Advent of Code 2025";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05"; };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
      compiler = "ghc984";
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskell.packages.${compiler};
        in rec {
          default = aoc2025;
          aoc2025 = haskellPackages.callPackage ./aoc2025.nix { };
        });

      devShell = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskell.packages.${compiler};
        in self.packages.${system}.default.env.overrideAttrs (oldAttrs: {
          buildInputs = (oldAttrs.buildInputs or [ ]) ++ [
            haskellPackages.cabal2nix
            haskellPackages.haskell-language-server
          ];
        }));
      # devShell = forAllSystems (system:
      #   let
      #     pkgs = nixpkgsFor.${system};
      #     haskellPackages = pkgs.haskell.packages.${compiler};
      #   in pkgs.mkShell {
      #     buildInputs = [
      #       self.packages.${system}.default.env
      #       haskellPackages.cabal2nix
      #       haskellPackages.haskell-language-server
      #     ];
      #   });
    };
}
