module Categories where

import Hakyll
import Html
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

categoryField' :: String -> Tags -> Context a
categoryField' = tagsFieldWith getCategory (renderLink "@") mconcat

categoryListField :: String -> Tags -> Context a
categoryListField key tags = field key (const $ renderList "@" tags)

categoryTitleField :: String -> Context a
categoryTitleField category = captionField $ "Articles in " <> capitalize category <> " category"
