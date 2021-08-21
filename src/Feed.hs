module Feed (feedRules) where

import Hakyll

feedRules :: Rules ()
feedRules = create ["rss.xml"] $ do
  route idRoute
  compile $ do
    loadAll "posts/**"
      >>= recentFirst
      >>= renderRss (feedConfiguration "All articles") feedCtx

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
