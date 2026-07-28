---
title: Re-using common parts of the Cabal configuration
tags: haskell, nix, Cabal
series: haskell-nix
language: english
---

When a your haskell repository consists of several Cabal libraries, it is very handy (from nix perspective) to have folders isolated from each other. Each sub-folder contains own `.cabal` file inside. That allows to declare derivations in very clean way:

```nix
haskellOverlay = hfinal: hprev:
  let
    build = name: source: hfinal.callCabal2nix "project-${name}" (gitignore-nix-src.gitignoreSource source) { };
    project = {
      project-types = build "types" ./src/types;
      project-core = build "core" ./src/core;
      project-tests = build "tests" ./tests;
    };
  in project // {
    project = builtins.attrValues project;
  };
```

That **project** `attrSet` is useful inside `shellFor` to discover dependencies from all of the libraries without the need to list all names again in `packages`.

```nix
haskellPkgs.shellFor {
  withHoogle = true;
  packages = p: p.project;
  genericBuilderArgsModifier = pkgs.lib.mapAttrs (_n: v: if pkgs.lib.isList v then builtins.filter (a: a == null || !(pkgs.lib.hasPrefix "project-" a.pname)) v else v);
}
```

Having multiple `.cabal` files is sure handy for `nix`, but it certainly hurts feelings of minimalist like me, because there has to be lots of repetitions. Even if you use `hpack` for generating `.cabal` files, you'll have to repeat things like:

- default extensions
- common dependencies (each of the libraries use)
- `ghc-options`, etc.

The solution I found is to use `!include` directive, which `hpack` natively supports. Put `package.common.yaml` file in the root of the repository, create a symlink to it in each of the sub-folders.

```yaml
# package.common.yaml
_all: &all
  version: "0.0.0.0"
  author: Alexander Shestakov
  license: AllRightsReserved

  language: GHC2024
  default-extensions:
    - DefaultSignatures
    - DeriveAnyClass
    - DerivingVia
    - NoImplicitPrelude
    - OverloadedLabels
    - OverloadedStrings
    - QuantifiedConstraints
    - TypeFamilies
    - UndecidableInstances

  dependencies:
    - base
    - deriving-aeson

  ghc-options: -Wall
  source-dirs: .

_executable: &executable
  ghc-options: -Wall -rtsopts "-with-rtsopts=-N" -threaded
  main: Main.hs
```

`hpack` will happily apply `YAML` merge keys defined in a `package.common.yaml` file. So each `package.yaml` becomes trivially short.

```yaml
# package.yaml from types folder
_common: !include "package.common.yaml"
<<: *all
name: project-types
library:
  dependencies:
    - aeson
    - ordered-containers
    - reflection
    - relude
    - scientific
```

To re-generate all the `.cabal` files, there is a short entry in project's `Justfile`.

```Makefile
hpack:
  find -iname package.yaml -exec hpack {} \;

build: hpack
  cabal build all
```