module Tags where

import Data.List
import Hakyll
import Html
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

tagsListField :: String -> Tags -> Context a
tagsListField key tags = field key (const $ renderList "#" tags)

tagTitleField :: String -> Context a
tagTitleField tag = captionField $ "Articles tagged #" <> tag
