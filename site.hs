{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Data.Either (fromRight)
import           Text.Regex (subRegex, mkRegex)
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Bifunctor
import qualified Data.Set as S
import           Hakyll.Contrib.LaTeX
import           Hakyll.Web.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Extensions
import           Text.Pandoc.Definition
import           Control.Monad (void)
import           Image.LaTeX.Render
import           Image.LaTeX.Render.Pandoc

--------------------------------------------------------------------------------
data PageStuff = PageStuff { pageDescription :: String
                           , pageName :: String
                           , author :: String
                           , authorEmail :: String
                           , pageAddress :: String }

zniecheta = PageStuff
    "Martwa skrzynka o matematyce, grach fabularnych i tematach pokrewnych."
    "Cierpliwość-zasadzka"
    "Rafał Szczerski"
    "fulgjon@pm.me"
    "http://cierpliwosc-zasadzka.pl"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = pageName zniecheta
    , feedDescription = pageDescription zniecheta
    , feedAuthorName  = author zniecheta
    , feedAuthorEmail = authorEmail zniecheta
    , feedRoot        = pageAddress zniecheta
    }

colours :: [(String, String)]
colours = [ ("foreground",   "#c5d4db") -- special
          , ("background",   "#090b0c")
          , ("cursorColor",  "#c5d4db")
          , ("color0",       "#111415") -- black
          , ("color8",       "#ebb692")
          , ("color1",       "#191d1f") -- red
          , ("color9",       "#bda6e0")
          , ("color2",       "#292f31") -- green
          , ("color10",      "#70abc1")
          , ("color3",       "#3f474b") -- yellow
          , ("color11",      "#efb58b")
          , ("color4",       "#5b666b") -- blue
          , ("color12",      "#e09140")
          , ("color5",       "#7c8a91") -- magenta
          , ("color13",      "#eea76b")
          , ("color6",       "#a3b5be") -- cyan
          , ("color14",      "#7fd2ef")
          , ("color7",       "#dbe4e8") -- white
          , ("color15",      "#e091b6")]

main :: IO ()
main = do
  renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/Concrete/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile $ do
          fmap compressCss <$>
            getResourceString >>= applyAsTemplate colourCtx

    match "fulgjon.pub" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/about.html"   defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*.markdown" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Wpisy z kategorii \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts


    match "posts/*.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithTransformM
            customReaderOptions
            defaultHakyllWriterOptions
            (renderFormulae themedFormulaOptions)
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Archiwum"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "" `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

colourCtx :: Context String
colourCtx = (mconcat $ (uncurry constField) <$> colours) <> defaultContext

themedFormulaOptions :: PandocFormulaOptions
themedFormulaOptions = PandocFormulaOptions
    { shrinkBy = 2
    , errorDisplay = displayError
    , formulaOptions = \case DisplayMath -> specMath "displaymath"; _ -> specMath "math"
    }

specMath :: String -> FormulaOptions
specMath s = FormulaOptions "\\usepackage{amsmath}\
    \\\usepackage{tikz}\
    \\\usepackage{amsfonts}\
    \\\usepackage{xcolor}\
    \\\definecolor{fg}{HTML}{c5d4db}\
    \\\everymath\\expandafter{\
        \\\the\\everymath \\color{fg}}\
    \\\everydisplay\\expandafter{\
        \\\the\\everydisplay \\color{fg}}" s 275

customReaderOptions = def { readerExtensions = extraReaderExts <> customReaderExts }
  where
    extraReaderExts = extensionsFromList [Ext_footnotes]
    customReaderExts = disableExtension Ext_implicit_figures $ pandocExtensions

