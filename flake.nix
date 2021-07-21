{
  description = "Blog Flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };

    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };

    nixpkgs = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
  };

  outputs = { self, flake-utils, pre-commit-hooks, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: rec {
            JuicyPixels-extra = super.JuicyPixels-extra.overrideDerivation (drv: { patchPhase = "sed -i 's/scaleBilinear_spec /scaleBilinear_spec/g' Codec/Picture/Extra.hs"; });
          };
        };

        blog = haskellPackages.callCabal2nix "blog" ./. { };

        cabal-fmt = haskellPackages.cabal-fmt;
      in
      rec {
        defaultApp = {
          type = "app";
          program = "${defaultPackage}/bin/blog";
        };
        defaultPackage = blog;

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint = { enable = true; };
              ormolu = { enable = true; };
              nixpkgs-fmt = { enable = true; };
              cabal = {
                enable = true;
                name = "cabal-fmt";
                entry = "${cabal-fmt}/bin/cabal-fmt --inplace";
                files = "\\.cabal$";
              };
            };
          };
        };

        devShell = ((
          (haskell.lib.addBuildTools defaultPackage [
            haskellPackages.fswatcher
            haskellPackages.apply-refact

            zlib

            haskell-language-server
            haskellPackages.cabal-fmt
            pre-commit-hooks.checks.${system}.ormolu
            pre-commit-hooks.checks.${system}.hlint
            pre-commit-hooks.checks.${system}.nixpkgs-fmt
            cabal-install
            haskellPackages.stan
          ])
        ).envFunc { }).overrideAttrs (f:
          {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          });
      });
}
