{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Combinator
import           Data.Either (fromRight)
import           Text.Regex (subRegex, mkRegex)
import           Data.Monoid (mappend)
import           Data.Maybe (fromJust)
import           Hakyll
import           Data.Bifunctor
import           Hakyll.Contrib.LaTeX
import           Hakyll.Web.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Extensions
import           Text.Pandoc.Definition
import           Data.Hex
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
    "rafal@szczerski.pl"
    "http://blog.szczerski.pl"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = pageName zniecheta
    , feedDescription = pageDescription zniecheta
    , feedAuthorName  = author zniecheta
    , feedAuthorEmail = authorEmail zniecheta
    , feedRoot        = pageAddress zniecheta
    }

type AColour = [(String, String)]
type HexColour = Int

data XResources = XResources { foreground  :: HexColour
                             , background  :: HexColour
                             , cursorColor :: HexColour
                             , color0      :: HexColour
                             , color8      :: HexColour
                             , color1      :: HexColour
                             , color9      :: HexColour
                             , color2      :: HexColour
                             , color10     :: HexColour
                             , color3      :: HexColour
                             , color11     :: HexColour
                             , color4      :: HexColour
                             , color12     :: HexColour
                             , color5      :: HexColour
                             , color13     :: HexColour
                             , color6      :: HexColour
                             , color14     :: HexColour
                             , color7      :: HexColour
                             , color15     :: HexColour}

parseXRes :: Parser [(String, String)]
parseXRes = concat <$> many1 matchSection

matchSection :: Parser [(String, String)]
matchSection = do
    matchHeading
    xs <- many1 matchColour
    optional eol
    return xs

matchHeading :: Parser ()
matchHeading = do
    char '!'
    space
    many1 alphaNum
    eol
    return ()

matchColour :: Parser (String, String)
matchColour = do
    string "*." 
    name <- many1 letter 
    dig  <- many digit
    char ':'
    spaces
    char '#' 
    x <- many1 alphaNum 
    string "\n"
    return (name ++ dig, '#':x)

eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

getColoursFromFile :: IO AColour
getColoursFromFile = fromRight [("fail", "fail")] <$>
    parseFromFile parseXRes "random.txt"

main :: IO ()
main = do 
  fColours <- getColoursFromFile
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
          let colourCtx = (mconcat $ 
                            (uncurry constField) <$>
                              fColours) <> defaultContext
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
        let mc   = tail $ fromJust $ lookup "foreground" fColours
            mdpi = 275
        compile $ pandocCompilerWithTransformM
            customReaderOptions
            defaultHakyllWriterOptions
            (renderFormulae $ defaultPandocFormulaOptions 
                              `customize` mathColour mc
                              `customize` FormulaOptions [] [] mdpi)
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])
            let pages = posts <> singlePages
                sitemapCtx =
                    constField "root" (pageAddress zniecheta) <> -- here
                    listField "pages" postCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

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
    constField "root" (pageAddress zniecheta) <>
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

customReaderOptions = def { readerExtensions = extraReaderExts <> customReaderExts }
  where
    extraReaderExts = extensionsFromList [Ext_footnotes]
    customReaderExts = disableExtension Ext_implicit_figures $ pandocExtensions

