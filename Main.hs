{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad (mplus)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Hakyll
import Hakyll.Images
import Hakyll.ShortcutLinks
import Hakyll.Web.Sass
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

deriving instance Eq a => Eq (Item a)

main :: IO ()
main = hakyllWith config $ do
  match "images/**" $ do
    route $ gsubRoute "images/" (const "previews/")
    compile $
      loadImage
        >>= ensureFitCompiler 512 512

    version "raw" $ do
      route idRoute
      compile copyFileCompiler

  match "css/*.sass" $ do
    route $ setExtension "css"
    compile (fmap compressCss <$> sassCompiler)

  match "vendor/**" $ do
    route idRoute
    compile copyFileCompiler

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      loadAll "posts/**"
        >>= recentFirst
        >>= renderRss (feedConfiguration "All articles") feedCtx

  tags <- buildTags "posts/**" (fromCapture "tags/*.html")
  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
  languages <- buildLanguages "posts/**" (fromCapture "languages/*.html")

  match "about.markdown" $ do
    route $ setExtension "html"
    compile $
      allShortcutLinksCompiler
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags categories languages)
        >>= relativizeUrls

  tagsRules tags $ \tag pat -> do
    let title = "Articles tagged " <> tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx =
            constField "title" title
              <> listField "posts" (postCtxWithTags tags categories languages) (return posts)
              <> postCtxWithTags tags categories languages

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules categories $ \tag pat -> do
    let title = "Articles from " <> capitalize tag <> " category"
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx =
            constField "title" title
              <> listField "posts" (postCtxWithTags tags categories languages) (return posts)
              <> postCtxWithTags tags categories languages

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules languages $ \tag pat -> do
    let title = "Articles in " <> capitalize tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx =
            constField "title" title
              <> listField "posts" (postCtxWithTags tags categories languages) (return posts)
              <> postCtxWithTags tags categories languages

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/**" $ do
    route $ setExtension "html"
    compile $ do
      let ctx = postCtxWithTags tags categories languages
      allShortcutLinksCompiler
        >>= saveSnapshot blogSnapshot
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      let ctx = postCtxWithTags tags categories languages
      posts <- recentFirst =<< loadAll "posts/**"

      let indexCtx =
            listField "posts" ctx (return posts)
              <> tagsListField "tagCloud" tags
              <> ctx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

capitalize :: String -> String
capitalize tag = toUpper (head tag) : tail tag

postCtxWithTags :: Tags -> Tags -> Tags -> Context String
postCtxWithTags tags categories languages =
  tagsField' "tags" tags
    <> categoryField' "category" categories
    <> languageField' "language" languages
    <> summaryField'
    <> categoryListField "categoryCloud" categories
    <> languagesListField "languageCloud" languages
    <> postCtx

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext

feedCtx :: Context String
feedCtx =
  mconcat
    [ bodyField "description",
      Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _ -> unContext mempty key,
      defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title =
  FeedConfiguration
    { feedTitle = "Shestakov Alex - " ++ title,
      feedDescription = "Personal blog of Shestakov Alex",
      feedAuthorName = "Shestakov Alex",
      feedAuthorEmail = "Maksar.mail@gmail.com",
      feedRoot = "https://maksar.github.io"
    }

blogSnapshot :: Snapshot
blogSnapshot = "blogSnapshot"

summaryField' :: Context String
summaryField' =
  teaserField "summary" blogSnapshot
    <> field "summary" (\item -> fromMaybe "" <$> getMetadataField (itemIdentifier item) "summary")

categoryField' :: String -> Tags -> Context a
categoryField' =
  tagsFieldWith getCategory (renderLink "@") mconcat

languageField' :: String -> Tags -> Context a
languageField' =
  tagsFieldWith getLanguage (renderLink "&") mconcat

tagsField' :: String -> Tags -> Context a
tagsField' =
  tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

renderLink :: String -> String -> Maybe FilePath -> Maybe H.Html
renderLink _ _ Nothing = Nothing
renderLink pre text (Just url) =
  Just $ do
    toHtml pre
    H.a ! href (toValue $ toUrl url) $ toHtml text

categoryListField :: String -> Tags -> Context a
categoryListField key tags =
  field key (const $ renderList tags)
  where
    renderList = renderTags makeLink unwords
    makeLink tag url _ _ _ = renderHtml $ do
      "@"
      H.a ! href (toValue url) $ toHtml tag

tagsListField :: String -> Tags -> Context a
tagsListField key tags =
  field key (const $ renderList tags)
  where
    renderList = renderTags makeLink unwords
    makeLink tag url _ _ _ = renderHtml $ do
      "#"
      H.a ! href (toValue url) $ toHtml tag

languagesListField :: String -> Tags -> Context a
languagesListField key tags = do
  field key (const $ renderList tags)
  where
    renderList = renderTags makeLink unwords
    makeLink tag url _ _ _ = renderHtml $ do
      "&"
      H.a ! href (toValue url) $ toHtml tag

getLanguage :: MonadMetadata m => Identifier -> m [String]
getLanguage identifier = do
  metadata <- getMetadata identifier
  return $
    fromMaybe [] $
      lookupStringList "language" metadata
        <> (map trim . splitAll "," <$> lookupString "language" metadata)

buildLanguages :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildLanguages = buildTagsWith getLanguage
