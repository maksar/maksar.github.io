---
title: Little handy QuasiQuoters
tags: haskell
language: english
---

List of very handy QuasiQuoters in Haskell, that makes routine programming tasks easier.

<!--more-->

<img src="/previews/quasi-quoters/logo.png" class="center" />

## [Data.Time.Clock.Duration.QQ](https://hackage.haskell.org/package/duration-0.2.0.0/docs/Data-Time-Clock-Duration-QQ.html)

Some Haskell libraries in Haskell tend to accept time durations as `Int`s (`timeout` from [base](https://hackage.haskell.org/package/base-4.20.0.1/docs/System-Timeout.html#v:timeout) wants microseconds, but `timeout` from [wai-extra](https://hackage.haskell.org/package/wai-extra-3.1.17/docs/Network-Wai-Middleware-Timeout.html#v:timeout) prefers seconds). That can be very error-prawn and annoying to work with. Set of QuasiQuoters from `Data.Time.Clock.Duration.QQ` package solves this problem in vert elegant way:

* Need to express 5 seconds worth of time, but in microseconds? `[µs|5s|]` (very cute usage of unicode, I must say).
* How long is 42 minutes in seconds? Trivial: `[s|42m|]`.
* Want to hardcode _a day_ as `NominalDiffTime`, but without doing `24*60*60` or `-- 24 hours` comment? Easy: `[t|1day|]`.

Many different time durations are [supported](https://hackage.haskell.org/package/duration-0.2.0.0/docs/Data-Time-Clock-Duration-Types.html#t:Time):

```haskell
data Time
  = Picosec  Rational
  | Nanosec  Rational
  | Microsec Rational
  | Millisec Rational -- ^ Denoted by @ms@, @msec@, @msecs@, @millisecond@, or @milliseconds@
  | Second   Rational -- ^ Denoted by @s@, @sec@, @secs@, @second@, or @seconds@
  | Minute   Rational -- ^ Denoted by @m@, @min@, @mins@, @minute@, or @minutes@
  | Hour     Rational -- ^ Denoted by @h@, @hr@, @hrs@, @hour@, or @hours@
  | Day      Rational -- ^ Denoted by @d@, @day@, or @days@
  | Week     Rational -- ^ Denoted by @w@, @week@, or @weeks@
  | Year     Rational -- ^ Denoted by @y@, @yr@, @yrs@, @year@, or @years@
```

## [Data.Time.QQ](https://hackage.haskell.org/package/time-qq-0.0.1.0/docs/Data-Time-QQ.html)

This QuasiQuoter is very handy when you need to put some human readable date in code, which expects `UTCTime`.

* Just a date, with time zeroed-out: `[utcIso8601|2048-12-01|]`.
* With particular time (including milliseconds): `[utcIso8601ms|2099-01-01T10:15:13.42324|]`.

## [Text.URI.QQ](https://hackage.haskell.org/package/modern-uri-0.3.6.1/docs/Text-URI-QQ.html) and [Network.URI.Static](https://hackage.haskell.org/package/network-uri-2.6.4.2/docs/Network-URI-Static.html)

Ever need to create a value of `URI` type (from [Text.URI](https://hackage.haskell.org/package/modern-uri-0.3.6.1/docs/Text-URI.html#t:URI) or from [Network.URI](https://hackage.haskell.org/package/network-uri-2.6.4.2/docs/Network-URI.html#t:URI))? Not very hard task to do, but type signatures assume parsing might fail:

```haskell
parseURI :: String -> Maybe URI
mkURI :: MonadThrow m => Text -> m URI
```

Hopefully, you can rely on QuasiQuoters: `[uri|https://www.google.com/|]` and get you `URI` checked at compile time.

## [Path.Posix](https://hackage.haskell.org/package/path-0.9.6/docs/Path-Posix.html)

Working with system paths requires some attention and love. I prefer to use stronger-typed [path](https://hackage.haskell.org/package/path) package. It allows to distinguish between absolute and relative paths, making code a but less error-prawn.

Set of QuasiQuoters allows to embed absolute and relative file system paths with ease:

* Files:
  * Relative: `[relfile|tests/Golden/trigger.schema|]`.
  * Absolute: `[absfile|/home/maksar/foo.txt|]`.
* Directories:
  * Relative: `[reldir|maksar|]`.
  * Absolute: `[absdir|/home/maksar|]`.

## [Text.Regex.Quote](https://hackage.haskell.org/package/lens-regex-0.1.3/docs/Text-Regex-Quote.html)

Regular expression QuasiQuoter, I think it is self-explanatory: `[r|run-([0-9a-f]{32})-sha-[0-9a-f]{40}|]`. However, there is a little twist, here is the quote from documentation:

> You can choose `Regex` type by changing imports.
>
> For example, the `exp` variable in the below example has the type of `Text.Regex.Posix.Regex`:
>
>```haskell
> import Text.Regex.Posix (Regex)
> exp = [r|hoge|]
> ```
>
> and, the exp variable in below example has the type of `Text.Regex.PCRE.Regex`:
> ```haskell
> import Text.Regex.PCRE (Regex)
> exp = [r|hoge|]
> ```


## [Data.String.Interpolate](https://hackage.haskell.org/package/string-interpolate-0.3.4.0/docs/Data-String-Interpolate.html)

That one is a Swiss army knife for string interpolation: `[i|run-#{run}-sha-#{sha}|]`.

Just compare it with alternatives:

* [Fmt](https://hackage.haskell.org/package/fmt) (which I prefer to use for a log messages because of [show brackets](https://hackage.haskell.org/package/fmt-0.6.3.0/docs/Fmt.html#g:6) and [combinators](https://hackage.haskell.org/package/fmt-0.6.3.0/docs/Fmt.html#g:7)):
  * Regular way `"run-" +| run |+ "-sha-" +| sha |+ ""`.
  * Format way `format "run-{}-sha-{}" run sha`.
* [Semigroup](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:-60--62-) way `"run-" <> run <> "-sha-" <> sha`.

It also has some flavours:

* [__i](https://hackage.haskell.org/package/string-interpolate-0.3.4.0/docs/Data-String-Interpolate.html#v:__i) An interpolator that handles indentation.
* [iii](https://hackage.haskell.org/package/string-interpolate-0.3.4.0/docs/Data-String-Interpolate.html#v:iii) An interpolator that strips excess whitespace.

## [Data.Aeson.QQ](https://hackage.haskell.org/package/aeson-qq-0.8.4/docs/Data-Aeson-QQ.html)

Unlike [Data.Aeson.QQ.Simple](https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson-QQ-Simple.html), this one allows interpolation of values. Very handy, no need to create custom type with `ToJSON` instance or use verbose json combinators from `Data.Aeson`.

```haskell
[aesonQQ|{
  experiment_ids: #{[experimentId]},
  filter: #{filterText},
  run_view_type: "ACTIVE_ONLY",
  max_results: 1,
  order_by: ["attributes.end_time DESC"]
}|])
```

## [Data.Aeson.Schema](https://hackage.haskell.org/package/aeson-schemas-1.4.2.1/docs/Data-Aeson-Schema.html)

If you love [jq](https://jqlang.github.io/jq/) like I do, you going to like this one.

Technically, function is called [get](https://hackage.haskell.org/package/aeson-schemas-1.4.2.1/docs/Data-Aeson-Schema.html#v:get) original [aeson-schemas](https://hackage.haskell.org/package/aeson-schemas) library, but I did seme renames for great good.

```haskell
type FromSchema = Data.Aeson.Schema.Object
jq = Data.Aeson.Schema.get
```

It allows to define your types as a json:

```haskell
type GitlabUserSchema = [schema|
  {
    id: Integer,
    username: Text,
    name: Text,
    web_url: Text
  }
|]

type GitlabUser = FromSchema GitlabUserSchema

type GitlabJobSchema = [schema|
  {
    id: Integer,
    status: GitlabStatus,
    name: Text,
    ref: Text,
    created_at: GitlabTime,
    started_at: Maybe GitlabTime,
    finished_at: Maybe GitlabTime,
    erased_at: Maybe GitlabTime,
    user: GitlabUser,
    commit: GitlabCommit,
    pipeline: Value,
    web_url: GitlabWebURL,
    project: Value
  }
|]

type GitlabJob = FromSchema GitlabJobSchema
```

And then extracts little bits and pieces out of it without lenses from [Data.Aeson.Lens](https://hackage.haskell.org/package/lens-aeson-1.2.3/docs/Data-Aeson-Lens.html) in a type-safe way. Isn't using `Data.Aeson.Lens` also type-safe you ask? Yeah, but with `Data.Aeson.Schema` you have **compile-type** checks!

Here is a real world example, that demonstrates the use of different QuasiQuoters and how it allows too make code more condensed and easier to reason about:

```haskell
monitorJob :: AppM m => GitlabProjectId -> GitlabStatus -> Integer -> m ()
monitorJob (GitlabProjectId projectId) targetStatus jobId = do
  GitlabAccessToken accessToken <- fetchAccessToken
  result <- timeout [µs|30min|] $ untilJust $ do
    result <- liftIO (request @GitlabJob $ getWith (defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)) [i|#{render gitlabHost}/api/v4/projects/#{projectId}/jobs/#{jobId}|])
    view #runtime >>= liftIO . \case
      CI -> putStr "\n"
      Local -> clearLine >> setCursorColumn 0
    case [jq|result.status|] of
      "canceled" -> reportFailure result
      "failed" -> reportFailure result
      status | status == targetStatus -> pure $ Just result
      _ -> do
        log D $ "Job "+|[jq|result.id|]|+" status is still "+|[jq|result.status|]|+"."
        liftIO $ do
          putStr "Waiting"
          replicateM_ 10 $ putChar '.' >> threadDelay [µs|1s|]
          pure Nothing

search :: AppM m => ExperimentName -> Maybe (Either GitTagName GitCommit) -> m (Maybe MlflowRun)
search experimentName git = do
  experiment <- getExperiment experimentName
  let
    experimentId = [jq|experiment.experiment_id|]
    baseConditions = ["tags.`mlflow.model-version` LIKE '%run-%-sha-%'", "attributes.status = 'FINISHED'"]
    filterCondition = case git of
      Nothing -> mempty
      Just (Left (GitTagName gitTag)) -> [i|attributes.run_name = '#{gitTag}'|]
      Just (Right (GitCommit gitCommit)) -> [i|tags.`mlflow.source.git.commit` = '#{gitCommit}'|]
    filterText = intercalate " AND " $ baseConditions <> [filterCondition]
  results <- liftIO $ request @MlflowGetRuns (postWith defaults [i|#{render mlflowHost}/ajax-api/2.0/mlflow/runs/search|]
    [aesonQQ|{
      experiment_ids: #{[experimentId]},
      filter: #{filterText},
      run_view_type: "ACTIVE_ONLY",
      max_results: 1,
      order_by: ["attributes.end_time DESC"]
    }|])
  pure $ head <$> (nonEmpty =<< [jq|results.runs?[]|])

```
