{
  description = "librarian";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # Dependency overrides go here
          };
        };

        nixpkgsOverlay = _final: _prev: {
          librarian = self.packages.${system}.librarian;
        };
      in
      rec {
        packages.librarian = haskellPackages.callCabal2nix "librarian" ./. { };

        defaultPackage = packages.librarian;

        overlays = nixpkgsOverlay;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = [
            self.defaultPackage.${system}.env
          ];
        };
      }
    );
}
