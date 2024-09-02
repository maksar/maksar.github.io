module Series where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Hakyll
import Html

getSeries :: (MonadMetadata m) => Identifier -> m (Maybe String)
getSeries = flip getMetadataField "series"

withMaybeOrEmpty :: (Alternative f) => Maybe t -> (t -> a) -> f a
withMaybeOrEmpty mv fun =
  case mv of
    Nothing -> empty
    Just v -> pure $ fun v

seriesField :: Tags -> Context a
seriesField tags =
  Context $
    const . \case
      "series" -> \item -> do
        name <- seriesName item
        guard $ not $ null name
        pure $ StringField name
      "seriesCurPos" -> \item -> do
        let identifier = itemIdentifier item
        others <- otherPostsInSeries item
        let index = elemIndex identifier others
        withMaybeOrEmpty index $ StringField . show . succ
      "seriesLength" -> \item -> do
        identifiers <- otherPostsInSeries item
        pure $ StringField $ show $ length identifiers
      "seriesUrl" -> \item -> do
        name <- seriesName item
        let tag = tagsMakeId tags name
        route <- getRoute tag
        withMaybeOrEmpty route $ StringField . toUrl
      _ -> const empty
  where
    seriesName item = do
      let identifier = itemIdentifier item
      series <- getSeries identifier
      pure $ fromMaybe empty series

    otherPostsInSeries item = do
      name <- seriesName item
      let posts = lookup name (tagsMap tags)
      pure $ fromMaybe empty posts

seriesTitleField :: String -> Context a
seriesTitleField serie = captionField $ capitalize serie

buildSeries ::
  (MonadMetadata m, MonadFail m) =>
  Pattern ->
  -- | Function for converting a given series name into an identifier for its page
  (String -> Identifier) ->
  m Tags
buildSeries pattrn makeId = do
  ids <- getMatches pattrn
  tagMap <- foldM addTags Map.empty ids
  let set' = Set.fromList ids
  inOrder <- (traverse . traverse) sortChronological (Map.assocs tagMap)
  pure $ Tags inOrder makeId (PatternDependency pattrn set')
  where
    addTags tagMap id' =
      maybe tagMap (\k -> Map.insertWith (++) k [id'] tagMap) <$> getSeries id'
