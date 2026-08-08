---
title: Three strategies for using custom Prelude
tags: haskell
language: english
---

There are several packages to replace haskell's built-in `Prelude` module. Each has own reasoning and underlying philosophy. The point is – custom `Prelude`s are a popular things. Lets discuss several ways to bring them into your project.

<!--more-->

# `base` without `Prelude`

Instead of depending on `base` package, you can simply replace it with `base-noprelude` in your `package.yaml` or `project.cabal` file. This package _mimics_ the structure of corresponding version of `base`, but does not provide/exposr the `Prelude` module. This unlocks very simple thing trick – to create a `Prelude` module directly in your project.

```haskell
module Prelude (module X) where

import Relude.Applicative as X
import Relude.Base as X
import Relude.Bool as X
import Relude.Container as X
import Relude.Debug as X
import Relude.File as X
import Relude.Foldable as X
import Relude.Function as X
import Relude.Functor as X
import Relude.List as X
import Relude.Monad as X
import Relude.Monoid as X
import Relude.Print as X
import Relude.String as X
import Relude.Extra.Map as X hiding (Key)

import UnliftIO.Async as X
import UnliftIO.Concurrent as X hiding (yield)
import UnliftIO.Directory as X
import UnliftIO.Environment as X hiding (getEnv)
import UnliftIO.Exception as X hiding (catchIO)
import UnliftIO.Foreign as X hiding (withArray)
import UnliftIO.IO as X
import UnliftIO.STM as X
import UnliftIO.Temporary as X

import Control.Lens.Getter as X
import Data.Generics.Labels ()

import Data.Aeson as X hiding (One)
import Data.Aeson.Text as X

import Network.HTTP.Client as X hiding (Proxy)
import Network.HTTP.Client.TLS as X

import System.FilePath as X hiding (isPathSeparator, pathSeparator)

import Conduit as X
import Data.Conduit.Combinators as X (iterM)
import Data.Conduit.List as X (chunksOf)
import Data.Conduit.TMChan as X
```

In regular haskell files of your project, you don't need to change anything, it _just works_ ©

Note, that `base-noprelude` package isn't well-supported, so you might need to bring it in from some fork:

```nix
base-noprelude = hfinal.callCabal2nix "base-noprelude" (pkgs.fetchFromGitHub { owner = "Holmusk"; repo = "base-noprelude"; rev = "jan/base-4.21.0.0"; hash = "sha256-sooplvtRIy7BLrxXIFFD/GbQY63+Vg8McTNzd8XHH90="; }) { };
```

# Mixins

Another fairly simple way to bring custom prelude is to use **mixins** feature from cabal (also well-supported with `hpack`):

```yaml
dependencies:
  - name: base
    mixin:
      - hiding (Prelude)
```

Then you can either import some package ([relude](https://hackage.haskell.org/package/relude-1.2.2.2#mixins) for example), which does provide custom `Prelude` module or define your own as before:

```haskell
{-# LANGUAGE PatternSynonyms #-}

module Prelude (module X, jq) where

import Data.Map.NonEmpty as X (NEMap)
import Data.Scientific as X
import Data.Set.NonEmpty as X (NESet)
import Data.Text as X (pattern Empty, stripPrefix, dropAround)
import Data.Time.Clock as X
import Data.Time.Clock.POSIX as X
import Data.Time.Format.ISO8601 as X (iso8601Show)
import Path as X
import Relude.Applicative as X
import Relude.Base as X hiding (natVal, someNatVal)
import Relude.Bool as X
import Relude.Container as X
import Relude.Debug as X
import Relude.Enum as X
import Relude.Exception as X
import Relude.File as X
import Relude.Foldable as X
import Relude.Function as X
import Relude.Functor as X
import Relude.List as X
import Relude.Monad as X hiding (reader)
import Relude.Monoid as X
import Relude.Nub as X
import Relude.Numeric as X
import Relude.String as X
import Text.Time.Pretty as X

import Data.TypeLits as X hiding (Abs)
import Data.Reflection as X

import Relude.Extra.Map as X
import Relude.Extra.Tuple as X
import Relude.Extra.Type as X

import Data.Aeson.QQ as X
import Data.Aeson.Schema as X (schema, unwrap, toMap)
import Data.String.Interpolate as X
import Data.Time.Clock.Duration as X
import Data.Time.QQ as X
import Fmt as X
import Fmt.Terminal as X
import Text.URI as X
import Text.URI.QQ as X

import Text.Regex.Lens as X
import Text.Regex.Posix as X (Regex)
import Text.Regex.Quote as X

import Colog as X (log, logException, pattern D, pattern I, pattern E, pattern W)
import System.Environment as X
import UnliftIO as X (MonadUnliftIO, tryAny, throwIO, handleAny)
import UnliftIO.Exception as X (throwString)

import Control.Lens.At as X
import Control.Lens.Fold as X
import Control.Lens.Getter as X
import Control.Lens.Setter as X hiding ((.=), setting)
import Data.Generics.Labels ()

import Data.Aeson.Schema qualified
import Language.Haskell.TH.Quote qualified

jq :: Language.Haskell.TH.Quote.QuasiQuoter
jq = Data.Aeson.Schema.get
```

# Custom project prelude

Nothing is wrong of course with just creating a custom `Prelude` for your project. First, I'd recommend to enable `NoImplicitPrelude` globally.

```yaml
default-extensions:
  - NoImplicitPrelude
```

And then you are free to even have several `Prelude`s to choose from:

- Minimal `Prelude`, useful for low-level common modules

```haskell
module Prelude.Minimal (module X) where

import Backends.Database as X

import Control.Lens as X (ASetter, at, filtered, folding, toListOf, (%=), (.=), (<>~))
import Control.Lens.Getter as X hiding (like)
import Data.Data as X (Data)
import Data.Data.Lens as X
import Data.Generics.Product as X hiding (IsList, field, param)
import Data.Generics.Wrapped as X
import Fmt as X hiding (format)
import GHC.TypeLits as X hiding (natVal, someNatVal)
import Prawda.Location as X

import DateRange as X
import JSON as X
import Time as X
import Val as X
```

- Main `Prelude`, useful for high-level abstract modules, which usually include utility modules (which used `Prelude.Minimal`)

```haskell
module Prelude.Main (module X) where

import Prelude.Minimal as X

import Vars.Types as X

import Config.TimeZoneConfig as X
import Programs as X
import PromisedLand as X
import Replays as X
import Utilities as X
```

# Bonus: pre-commit hook

It might be useful to add `git` pre-commit hook to prevent developers from using stock `Prelude` by accident (in qualified or non-qualifies way).

```nix
no-prelude = {
  enable = true;
  entry = ''bash -c 'grep -E -n -H "^import Prelude($|\s)" "$@" && exit 1 || exit 0' --'';
  types = [ "haskell" ];
};
```