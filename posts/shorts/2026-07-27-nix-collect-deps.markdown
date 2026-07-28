---
title: Collecting all dependencies for haskell nix project
tags: haskell, nix, Cabal
series: haskell-nix
language: english
---

If you are developing a library or a binary with a single cabal file in a repository, nix integration is very straightforward:

```nix
devShell = haskellPkgs.shellFor {
  withHoogle = true;
  packages = p: with p; [ your-project-name ];
  nativeBuildInputs = with haskellPkgs; [ cabal-install hpack haskell-language-server ];
};
```

But what to do if there are several cabal libraries in your repository and you manage dependencies for them separately... What, in that case, you have to put into `packages = p: with p; [ ... ];` place?

The naïve answer is – just put all of your libraries there. But doing that can play a bad trick, because libraries does depend on each other. So it you have `your-project-lib-a` and `your-project-lib-b`, which depends on `your-project-lib-a` – then `your-project-lib-a` will end up in the list of dependencies that `shellFor` wants to have built with nix in order to provision your `nix-shell`.

It creates a strange self-dependant loop, in which the very fact of entering `nix-shell` **requires** to be built `your-project-lib-` with nix...

The trick is to use somewhat not-well documented `genericBuilderArgsModifier` feature of the `shellFor` in nix – it allows to filter-out some dependencies.

```nix
devShell = haskellPkgs.shellFor {
  withHoogle = true;
  packages = p: with p; [ your-project-lib-a your-project-lib-b ];
  genericBuilderArgsModifier = pkgs.lib.mapAttrs (_n: v: if pkgs.lib.isList v then builtins.filter (a: a == null || !(pkgs.lib.hasPrefix "your-project-lib-" a.pname)) v else v);
  nativeBuildInputs = with haskellPkgs; [ cabal-install hpack haskell-language-server ];
};
```

So when `shellFor` iterates over `[ your-project-lib-a your-project-lib-b ]` to discover dependencies of each of them, it will ignore every dependency which starts from `your-project-lib-` prefix.
