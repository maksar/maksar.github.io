module Html where

import Data.Char
import Hakyll
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

renderLink :: String -> String -> Maybe FilePath -> Maybe H.Html
renderLink _ _ Nothing = Nothing
renderLink pre text (Just url) =
  Just $ do
    toHtml pre
    H.a ! href (toValue $ toUrl url) $ toHtml text

renderList :: H.Html -> Tags -> Compiler String
renderList symbol = renderTags (makeLink symbol) unwords

makeLink :: (H.ToValue url, H.ToMarkup tag) => H.Html -> tag -> url -> count -> min -> max -> String
makeLink symbol tag url _ _ _ = renderHtml $ do
  symbol
  H.a ! href (toValue url) $ toHtml tag

capitalize :: String -> String
capitalize tag = toUpper (head tag) : tail tag

captionField :: String -> Context a
captionField = constField "title"
