---
title: HLS support for multi-target Cabal projects
tags: HLS, haskell, Cabal
language: english
---

Some time ago I read the [article](https://jade.fyi/blog/cabal-test-dev-trick/) about neat `Cabal` trick to solve `HLS`'s inability to work with multi-target Cabal projects. Basically, at the current moment, HLS fails to do a decent jobs when you have:

- multiple binaries in the same Haskell project
- multiple internal libraries

Some time ago in one of the projects we had this solved with custom `hie.yaml` files, which pointed to individual `Cabal` components.

```yaml
cradle:
  multi:
    - path: "./"
      config:
        cradle:
          bios:
            shell: ./hie.sh
```

Script to handle that was **very** complicated and full of project specific details:

```sh
#!/bin/bash
just hpack 1>&2
(
  echo "-hide-all-packages"
  echo "-XAllowAmbiguousTypes"
  echo "-packagepolysemy-plugin"
  yq -r '._all."dependencies" | map("-package" + .) | join("\n")' package.common.yaml
  yq -r '._all."default-extensions" | map("-X" + .) | join("\n")' package.common.yaml
  yq -r '._all."ghc-options" | join("\n")' package.common.yaml

  for autogenDir in $(find dist-newstyle -name autogen -type d -not -path "*/x/*" -not -path "*/t/*")
  do
    echo "-i${autogenDir}"
    find "$autogenDir" -name "*.hs" -type f
  done
  for packageFile in $(find . -maxdepth 3 -name "package.yaml")
  do
    packageDir="$(dirname $packageFile)/"
    cat package.common.yaml $packageFile | yq -r --arg path "$packageDir" --arg noTests "$NO_TEST" 'if 0 == ($noTests | length) then . else del(.tests) end | .. ."dependencies"? | select(.) | map(select(. | startswith("project-name") | not)) | flatten | .[] | "-package\(.)"'
    for sourceDir in $(cat package.common.yaml $packageFile | yq -r --arg path "$packageDir" --arg noTests "$NO_TEST" 'if 0 == ($noTests | length) then . else del(.tests) end | .. ."source-dirs"? | select(.) | flatten | .[] | "\($path)\(.)"')
    do
      # TODO: Remove when we don't have nonexistent lib in test package
      if [ -d $sourceDir ]; then
        echo "-i${sourceDir}"

        # Only match on directories that look like Haskell modules
        find $sourceDir -name "*.hs" | grep -v "${sourceDir}/[^A-Z]"
      fi
    done
  done
) >> $HIE_BIOS_OUTPUT

(
  find . -maxdepth 3 -name "package.yaml"
  echo "package.common.yaml"
  echo "default.nix"
  echo "shell.nix"
  echo "project-name.cabal"
) >> $HIE_BIOS_DEPS
```


But this wasn't ideal solution (I mean, just look at the file above...) as we still had to come-up with custom GHCi script (`ghci.sh`) to start REPL:

```sh
#!/bin/bash
# this will load all components to a single GHCI session
opts=$(HIE_BIOS_OUTPUT=/dev/stdout HIE_BIOS_DEPS=/dev/null ./hie.sh | tr '\n' ' ')
exec ghci -hidir $PWD/dist-ghc/ -odir $PWD/dist-ghc/ $opts -fobject-code -Wall -O0 +RTS -N -A128M -RTS
```

Article suggested to create an artificial Cabal component to use specifically for HLS/REPL purposes. Finally I had time and opportunity to experiment with approach in automatic manner. So I created small [jq](https://jqlang.github.io/jq/) script to update `package.yaml` on the fly:

```jq
.name as $name |
(.executables + (.tests | del(.hls)) + {library}) as $sources |
def collect($f): $sources | map((.[$f] // [])[]) | unique | sort;
.benchmarks.hls =
(.tests.test + (["source-dirs", "default-extensions", "ghc-options", "dependencies"] | map({(.): collect(.)}) | add) | (.dependencies -= [$name]))
```

It is integrated with out [Justfile](https://github.com/casey/just):

```Makefile
default:
  just test

clean: hpack
  cabal clean

build: hpack
  cabal build all

test args=(""): build
  cabal test test --test-show-details=streaming --test-options='{{args}}'

hls:
  yq -f hls.jq -y -i package.yaml

hpack: hls
  fd package.yaml -x hpack

gen: hpack
  echo ":set -Wno-missing-home-modules" > ghci.test
  cat package.yaml | yq -r '.benchmarks.hls.dependencies | map(":set -package " + .) | join("\n")' >> ghci.test
  cat package.yaml | yq -r '.benchmarks.hls."source-dirs" | map(":set -i" + .) | join("\n")' >> ghci.test
  cat package.yaml | yq -r '.benchmarks.hls.main | ":l " + .' >> ghci.test
  cat package.yaml | yq -r '.benchmarks.hls."ghc-options" | map(":set " + .) | join("\n")' >> ghci.test

repl: gen
  (echo ":set -XQuasiQuotes" && echo "import Prelude" && cat) | cabal repl benchmark:hls

watch: gen
  ghciwatch --command "cabal repl benchmark:hls --repl-no-load" --after-startup-ghci ':script ghci.test' --watch "." --restart-glob "package.yaml" --before-startup-shell "hpack" --test-ghci "Main.main" --no-interrupt-reloads --clear
```

The reason for using `benchmark` instead of `test` is: by default, `cabal build all` will build all tests components, but not benchmarks. Moreover, `nix build` for the package would not bother building `benchmark`s by default. So this approach isn't prone to spending more time on CI.

Bonus: [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) is pretty cool, check it out!