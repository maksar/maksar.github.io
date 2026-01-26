{
  description = "Blog Flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs = { url = "github:nixos/nixpkgs/26.05-pre"; };
  };

  outputs = { self, flake-utils, pre-commit-hooks, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; };
      let
        haskellPackages = pkgs.haskell.packages.ghc912.override {
          overrides = self: super: {
            pandoc = haskell.lib.dontCheck super.pandoc;
            unicode-data = haskell.lib.doJailbreak super.unicode-data_0_8_0;
            hlibsass = self.callHackageDirect
              {
                pkg = "hlibsass";
                ver = "0.1.10.3";
                sha256 = "sha256-N1FtdDYp572kGMkhmOqpSPVJN+Ew/ug4RWnmiMBngM8=";
              }
              { };
          };
        };

        blog = haskellPackages.callCabal2nix "blog" ./. { };
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
            imagemagick
            zlib

            haskellPackages.cabal-install
            haskellPackages.haskell-language-server

            pre-commit-hooks.checks.${system}.hlint
            pre-commit-hooks.checks.${system}.nixpkgs-fmt
            pre-commit-hooks.checks.${system}.ormolu
          ])
        ).envFunc { }).overrideAttrs (f: { inherit (self.checks.${system}.pre-commit-check) shellHook; });
      });
}
