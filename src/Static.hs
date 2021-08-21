module Static where

import Hakyll
import Hakyll.Favicon
import Hakyll.Images
import Hakyll.Web.Sass

staticRules :: Rules ()
staticRules = do
  match "templates/**" $ compile templateBodyCompiler

  faviconsRules "images/favicon.svg"

  match ("images/**" .&&. complement "**/*.svg") $ do
    route $ gsubRoute "images/" (const "previews/")
    compile $
      loadImage >>= ensureFitCompiler 512 512

    version "raw" $ do
      route idRoute
      compile copyFileCompiler

  match "css/*.sass" $ do
    route $ setExtension "css"
    compile (fmap compressCss <$> sassCompiler)

  match "vendor/**" $ do
    route idRoute
    compile copyFileCompiler
