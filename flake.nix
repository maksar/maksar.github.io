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
            JuicyPixels-extra = self.callHackageDirect
              {
                pkg = "JuicyPixels-extra";
                ver = "0.5.2";
                sha256 = "sha256-NoMF4US7mqbHRtVAqssvFjHMr2iXBR8DFrMgKhRHY9w=";
              }
              { };
            hakyll-shortcut-links = haskell.lib.doJailbreak (haskell.lib.unmarkBroken (self.callHackageDirect
              {
                pkg = "hakyll-shortcut-links";
                ver = "0.1.0.1";
                sha256 = "sha256-AW77lw0WlpVh3aXrsbQHW2g+Iuqvjf1xKPA/+Ds8XzU=";

              }
              { }));
            shortcut-links = haskell.lib.dontCheck (haskell.lib.unmarkBroken (self.callHackageDirect
              {
                pkg = "shortcut-links";
                ver = "0.5.1.1";
                sha256 = "sha256-d5sHE744bjK69e7QvWafew8wHqYQwS9QR7bSYdSBGlU=";

              }
              { }));
            hakyll-images = self.callHackageDirect
              {
                pkg = "hakyll-images";
                ver = "1.1.0";
                sha256 = "sha256-K35fkehx37olJKNQxyhQyLTvdfMUSrar0LFaYFOfQAE=";
              }
              { };
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
              blog = {
                enable = true;
                name = "blog";
                entry = "${cabal-install}/bin/cabal run blog rebuild";
                files = "^.*$";
                pass_filenames = false;
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
        ).envFunc { }).overrideAttrs
          (f: { inherit (self.checks.${system}.pre-commit-check) shellHook; });
      });
}
