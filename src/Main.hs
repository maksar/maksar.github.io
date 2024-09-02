{-# LANGUAGE DataKinds #-}

module Main where

import Categories
import Config
import Data.List
import qualified Data.Set as S
import Feed
import Hakyll
import Hakyll.Favicon
import Hakyll.ShortcutLinks
import Languages
import Series
import Static
import System.FilePath
import Tags

deriving instance (Eq a) => Eq (Item a)

main :: IO ()
main = hakyllWith config $ do
  staticRules
  feedRules

  tags <- buildTags "posts/**" (fromCapture "tags/*/index.html")
  categories <- buildCategories "posts/**" (fromCapture "categories/*/index.html")
  languages <- buildLanguages "posts/**" (fromCapture "languages/*/index.html")
  series <- buildSeries "posts/**" (fromCapture "series/*/index.html")
  empty <- buildTags "" (const "")
  let context = postCtxWithTags tags categories languages series

  match "about.markdown" $ do
    route cleanRoute
    compile $
      allShortcutLinksCompiler
        >>= defaultCompiler context

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      posts <- loadPosts pat
      let ctx = tagTitleField tag <> postsField posts (postCtxWithTags empty categories languages series) <> context

      makeItem ""
        >>= loadAndApplyTemplate "templates/tags/default.html" ctx
        >>= defaultCompiler ctx

  tagsRules categories $ \cetegory pat -> do
    route idRoute
    compile $ do
      posts <- loadPosts pat
      let ctx = categoryTitleField cetegory <> postsField posts (postCtxWithTags tags empty empty series) <> context

      makeItem ""
        >>= loadAndApplyTemplate (fromFilePath ("templates/categories/" <> cetegory <> ".html")) ctx
        >>= defaultCompiler ctx

  tagsRules languages $ \language pat -> do
    route idRoute
    compile $ do
      posts <- loadPosts pat
      let ctx = languageTitleField language <> postsField posts context <> context

      makeItem ""
        >>= loadAndApplyTemplate (fromFilePath ("templates/languages/" <> language <> ".html")) ctx
        >>= defaultCompiler ctx

  tagsRules series $ \serie pat -> do
    route idRoute
    compile $ do
      posts <- loadPosts pat
      let ctx = seriesTitleField serie <> postsField posts (postCtxWithTags tags empty empty series) <> context

      makeItem ""
        >>= loadAndApplyTemplate "templates/series/default.html" ctx
        >>= defaultCompiler ctx

  match "posts/**" $ do
    route cleanRoute
    compile $ do
      allShortcutLinksCompiler
        >>= saveSnapshot blogSnapshot
        >>= loadAndApplyTemplate "templates/post.html" context
        >>= defaultCompiler context

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadPosts ("posts/**" .&&. complement "posts/WAT/**")
      let ctx = postsField posts context <> context

      getResourceBody
        >>= applyAsTemplate ctx
        >>= defaultCompiler ctx

postCtxWithTags :: Tags -> Tags -> Tags -> Tags -> Context String
postCtxWithTags tags categories languages series =
  tagsField' "tags" tags
    <> categoryField' "category" categories
    <> categoryListField "categories" categories
    <> languageField' "language" languages
    <> languagesListField "languages" languages
    <> tagsListField "tagCloud" tags
    <> seriesField series
    <> teaserField "summary" blogSnapshot
    <> faviconsField
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

blogSnapshot :: Snapshot
blogSnapshot = "blogSnapshot"

defaultCompiler :: Context a -> Item a -> Compiler (Item String)
defaultCompiler ctx item =
  loadAndApplyTemplate "templates/default.html" ctx item
    >>= relativizeUrls
    >>= cleanIndexUrls

postsField :: [Item a] -> Context a -> Context b
postsField posts ctx = listField "posts" ctx (return posts)

loadPosts pat = recentFirst =<< loadAll pat

cleanRoute :: Routes
cleanRoute = customRoute $ \iden -> takeDirectory (toFilePath iden) </> takeBaseName (toFilePath iden) </> takeFileName indexHtml

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = pure . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = pure . fmap (replaceAll indexHtml (const ""))

cleanIndex :: String -> String
cleanIndex url
  | indexHtml `isSuffixOf` url = take (length url - length indexHtml) url
  | otherwise = url

indexHtml :: String
indexHtml = "/index.html"
