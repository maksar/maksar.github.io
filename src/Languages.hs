module Languages where

import Data.Maybe
import Hakyll
import Html
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

languagesListField :: String -> Tags -> Context a
languagesListField key tags = field key (const $ renderList "&" tags)

languageField' :: String -> Tags -> Context a
languageField' = tagsFieldWith getLanguage (renderLink "&") mconcat

getLanguage :: (MonadMetadata m) => Identifier -> m [String]
getLanguage identifier = do
  metadata <- getMetadata identifier
  return $
    fromMaybe [] $
      lookupStringList "language" metadata
        <> (map trim . splitAll "," <$> lookupString "language" metadata)

buildLanguages :: (MonadMetadata m) => Pattern -> (String -> Identifier) -> m Tags
buildLanguages = buildTagsWith getLanguage

languageTitleField :: String -> Context a
languageTitleField language = captionField $ "Articles in " <> capitalize language
