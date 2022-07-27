{
  description = "librarian";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        github = owner: repo: rev: sha256:
          builtins.fetchTarball { inherit sha256; url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };
        sources = {
          autodocodec = github "NorfairKing" "autodocodec" "ea32711af97a46ee34de9ac488060f626961d08d" "04n744fl25vh2h0mvgd60lkj6hc2n0p50sjxiy8v0d456fclz4lb";
          sydtest = github "NorfairKing" "sydtest" "e2d8701749023e9fa4703a85d831e0d3c18fda79" "1k17avkpgnk766l34jlnz9ydn16l9nllc56vm4hrnxfc01dbxys7";
        };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskell.packages.ghc923.override {
          overrides = hself: hsuper: {
            # safe-coloured-text = hsuper.safe-coloured-text_0_2_0_1;
            autodocodec = jailbreakUnbreak (hsuper.callCabal2nix "autodocodec" "${sources.autodocodec}/autodocodec" { });
            autodocodec-yaml = jailbreakUnbreak (hsuper.callCabal2nix "autodocodec-yaml" "${sources.autodocodec}/autodocodec-yaml" { });
            sydtest = jailbreakUnbreak (hsuper.callCabal2nix "sydtest" "${sources.sydtest}/sydtest" { });
            validity-aeson = jailbreakUnbreak hsuper.validity-aeson;
          };
        };
      in
      rec
      {
        packages.librarian = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "librarian" ./. rec {
            # Dependency overrides go here
          };

        defaultPackage = packages.librarian;

        overlay = final: prev: {
          ghc = final.haskellPackages.ghcWithHoogle (p:
            with p; [
              text
              hspec
              hspec-discover
            ]);
        };

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            haskell-ci
            feedback
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
