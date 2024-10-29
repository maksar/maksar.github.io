{
  description = "Blog Flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs = { url = "github:NixOS/nixpkgs/release-24.05"; };
  };

  outputs = { self, flake-utils, pre-commit-hooks, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs
        {
          inherit system;
          config = { allowBroken = true; };
        };
      let
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            hakyll-shortcut-links = haskell.lib.doJailbreak super.hakyll-shortcut-links;
            shortcut-links = haskell.lib.doJailbreak super.shortcut-links;
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
            haskellPackages.fswatcher
            haskellPackages.haskell-language-server

            pre-commit-hooks.checks.${system}.hlint
            pre-commit-hooks.checks.${system}.nixpkgs-fmt
            pre-commit-hooks.checks.${system}.ormolu
          ])
        ).envFunc { }).overrideAttrs (f: { inherit (self.checks.${system}.pre-commit-check) shellHook; });
      });
}
