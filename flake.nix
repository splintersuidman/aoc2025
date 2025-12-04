{
  description = "Flake for Advent of Code 2025";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11"; };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskellPackages;
        in rec {
          default = aoc2025;
          aoc2025 = haskellPackages.callPackage ./aoc2025.nix { };

          shell = self.packages.${system}.default.env.overrideAttrs (oldAttrs: {
            buildInputs = (oldAttrs.buildInputs or [ ]) ++ [
              haskellPackages.cabal2nix
              haskellPackages.haskell-language-server
            ];
          });
        });

      devShell = forAllSystems (system: self.packages.${system}.shell);
    };
}
