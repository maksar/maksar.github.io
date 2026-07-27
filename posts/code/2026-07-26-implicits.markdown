---
title: Actually useful implicit arguments
tags: haskell
language: english
---

[Implicit parameters](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/implicit_parameters.html) is an underrated feature of haskell. Many programmers avoid it as it reminds of or somehow is associated with _global state_ in the minds. In this article I'll try to demonstrate how implicit parameters can reduce boilerplate in the code, acting like a `ReaderT` without the need of using `ask`.

<!--more-->

Lets say you have a configuration data structure `ProjectConfig` defined like this:

```haskell
data ProjectConfig = ProjectConfig
  { hitlogsStart :: SlidingDay
  , hitlogsLookback :: CalendarDiffDays
  , clientTimeZone :: TimeZoneConfig
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via SnakifyJSON ProjectConfig
```

Where `TimeZoneConfig` is a very simple data structure.

```haskell
data TimeZoneConfig = TimeZoneConfig
  { abbreviation :: Text
  , ianaCode :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via SnakifyJSON TimeZoneConfig
```

And `SlidingDay` is a little bit more complex one, with custom `FromJSON` instance for paring. It allows to either specify an `AbsoluteDate` exact date or specify a `RelativeDays` (to the current day) shift in days. That might be useful in different environments:
- in production you want to always use an exact date;
- in staging environment it might be useful to specify `- 1 day` relative value.

```haskell
data SlidingDay
  = RelativeDays Integer
  | AbsoluteDate Day
  deriving stock (Eq, Generic, Show)

instance FromJSON SlidingDay where
  parseJSON = withText "SlidingDay" $ \t -> case toString t of
    '-' : rest -> case stripSuffix "days" (strip $ toText rest) >>= readMaybe . toString of
      Just v -> pure $ RelativeDays v
      Nothing -> fail $ "Invalid SlidingDay format: " +| t |+ ""
    _ -> AbsoluteDate <$> parseJSON (String t)

instance ToJSON SlidingDay where
  toJSON = \case
    RelativeDays n -> String $ "-" <> show n <> " days"
    AbsoluteDate day -> toJSON day

fromSliding :: (MonadIO m) => SlidingDay -> m Day
fromSliding = \case
  RelativeDays n -> do
    UTCTime {utctDay} <- liftIO Time.getCurrentTime
    pure $ subtractGregorianDurationRollOver (scaleCalendarDiffDays (fromIntegral n) calendarDay) utctDay
  AbsoluteDate day -> pure day
```

The goal is to make `ProjectConfig` available anywhere in the application code (function in `ProjectMonad a` monad). Standard approach would be to let `MonadProject` have instance of `MonadReader ProjectConfig`. It is relatively easy to write, something like this in `mtl` style:

```haskell
newtype ProjectMonad a = ProjectMonad (ReaderT ProjectConfig IO a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadIO, MonadIO, MonadReader ProjectConfig)
```

Somewhere close to application's `main` function, you'd parse the config, fill in fields of the `ProjectConfig` and run the application code with a `runReaderT`.

```haskell
runProject :: ProjectConfig -> ProjectMonad a -> IO a
runProject config (ProjectMonad app) = runReaderT app config
```

However, this comes with an obligation to use monadic [`view`](https://hackage-content.haskell.org/package/lens-5.3.6/docs/Control-Lens-Getter.html#v:view) getters or even implement custom accessor functions, which would have to be monadic too. So in application code, you'll have to rely on `<-` every time you want to get a field from a project configuration data structure.

And that is perfectly fine, if you are experienced haskell developer, everything looks normal and familiar. But what if there are less experienced (in haskell) people, who have to work with the project's code or, at least, being able to read and understand the code inside `ProjectMonad a` functions? Type signatures like `view :: MonadReader s m => Getting a s a -> m a` or type class declarations like `class Monad m => MonadReader r (m :: Type -> Type) | m -> r` can be very scary to novices. Implicit parameters to the rescue!

We define a `HasProjectConfig` constraint, which _inherits_ `HasTimeZoneConfig` constraint. Both of them just demand several implicit parameters to be present. In haskell, tuple of a constraints can be _nested_, very handy.

```haskell
type HasProjectConfig =
  ( ?hitlogsStart :: Day
  , ?hitlogsLookback :: CalendarDiffDays
  , HasTimeZoneConfig
  )

type HasTimeZoneConfig =
  ( ?clientTimeZoneAbbreviation :: Text
  , ?clientTimeZoneIanaCode :: Text
  )
```

Then we define `provideProjectConfig` function, which being given a computation requiring a `HasProjectConfig` constraint and a `ProjectConfig` – runs the computation by filling in blanks – required identifiers with question marks. Notice, that this helper is also responsible for using the `fromSliding` function, which has to be in `IO` because ir has to know current date. The rest of application code just sees the final calculated code. So we don't have to use the separate data structure (either manual or via type families over parametrized data structures) for parsing the config and for using it.

```haskell
provideProjectConfig :: (MonadIO m) => ((HasProjectConfig) => m a) -> ProjectConfig -> m a
provideProjectConfig go ProjectConfig {..} = do
  hitlogsStartDay <- fromSliding hitlogsStart
  let
    ?hitlogsStart = hitlogsStartDay
    ?hitlogsLookback = hitlogsLookback
    ?tableClientStartDate = tableClientStartDate
   in
    provideTimeZone go clientTimeZone

provideTimeZone :: ((HasTimeZoneConfig) => a) -> TimeZoneConfig -> a
provideTimeZone go TimeZoneConfig {..} =
  let
    ?clientTimeZoneAbbreviation = Text.toLower abbreviation
    ?clientTimeZoneIanaCode = ianaCode
   in
    go
```

Finally, helper `withProjectConfig` function, which serves the purpose of the `runProject` from before. It should be called at the very top of the application structure (close to `main`), to parse the config, fill in the implicit parameter holes and run the application.

```haskell
withProjectConfig :: (MonadIO m) => ((HasProjectConfig) => m a) -> m a
withProjectConfig app = withParsedConfig $ provideProjectConfig app
  where
    withParsedConfig :: forall a b m. (MonadIO m, FromJSON a, Typeable a) => (a -> m b) -> m b
```

Using this approach, application logic doesn't have to use monadic accessors, if you need to access something from the `ProjectConfig`, you can easily do it in any place of the code, even from non-monadic functions:

```haskell
someApplicationLogic :: (HasProjectConfig) => ProjectMonad [Client]
someApplicationLogic = do
  logDebug $ "Searching clients from " <> ?clientTimeZoneAbbreviation <> " time zone"
  executeQuery $ "SELECT * FROM clients WHERE timeZone = ?" clientTimeZoneCodeForSQL

clientTimeZoneCodeForSQL :: (HasProjectConfig) => Text
clientTimeZoneCodeForSQL = Text.toTitle ?clientTimeZoneIanaCode
```
