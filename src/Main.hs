{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Char
  ( toLower,
  )
import Data.List
  ( find,
    intercalate,
    isPrefixOf,
    isSuffixOf,
  )
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Monoid (mappend)
import qualified Data.Set as S
import qualified Data.Text as T
import Hakyll
import qualified Hakyll.Core.Store as Store
import System.FilePath.Posix
  ( (</>),
    joinPath,
    normalise,
    splitDirectories,
    splitPath,
    takeBaseName,
    takeDirectory,
    takeFileName,
  )
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk

--------------------------------------------------------------------------------
config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "public",
      deployCommand = "bash deploy.sh",
      storeDirectory = ".hakyll-cache"
    }

main :: IO ()
main = hakyllWith config $ do
  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler
  match "documents/**" $ do
    route idRoute
    compile copyFileCompiler
  match "pictures/**" $ do
    route idRoute
    compile copyFileCompiler
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/**" $ do
    route idRoute
    compile copyFileCompiler
  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler
  match (fromList ["about.md"]) $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  match ("posts/*.org") $ do
    route tempRoute
    compile $ getResourceString >>= orgCompiler
  match (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org") $ do
    route $ metadataRoute $ titleRoute
    compile $
      -- getResourceString
      --   >>= convertUrlCompiler >>=
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  create ["404.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            constField "body" "404 Not Found"
              `mappend` constField "title" "404 Not Found"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  create ["403.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            constField "body" "403 Forbidden"
              `mappend` constField "title" "403 Forbidden"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  create ["archives.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org")
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org")
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match "templates/*" $ compile templateBodyCompiler
  match ("node_modules/katex/dist/katex.*" .||. "node_modules/katex/dist/contrib/auto-render.*" .||. "node_modules/katex/dist/fonts/**") $ do
    route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
    compile copyFileCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` mathCtx
    `mappend` defaultContext

enableKatexMin :: Bool
enableKatexMin = True

enableMathml :: Bool
enableMathml = False

mathjaxScript :: String
mathjaxScript = "<script type=\"text/javascript\" async src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML\"></script>"

katexScript :: String
katexScript =
  let min = if enableKatexMin then ".min" else ""
   in "<link rel=\"stylesheet\" href=\"/vendor/katex/katex" ++ min ++ ".css\">\n<script defer type=\"text/javascript\" src=\"/vendor/katex/katex" ++ min ++ ".js\"></script>\n<script defer type=\"text/javascript\" src=\"/vendor/katex/contrib/auto-render" ++ min ++ ".js\" onload=\"renderMathInElement(document.body);\"></script>"

mathCtx :: Context String
mathCtx = field "mathRender" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return ""
  return $
    case fmap (map toLower) $ lookupString "math" metadata of
      Just "mathjax" -> mathjaxScript
      Just "mathml" -> ""
      Just "no" -> ""
      Just "off" -> ""
      Just "0" -> ""
      _ -> katexScript

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  let mathExtensions =
        extensionsFromList
          [ Ext_tex_math_dollars,
            Ext_tex_math_single_backslash,
            Ext_tex_math_double_backslash,
            Ext_latex_macros,
            Ext_citations,
            Ext_footnotes
          ]
  let defaultExtensions = writerExtensions defaultHakyllWriterOptions
  let newExtensions = defaultExtensions <> mathExtensions
  identifier <- getUnderlying
  mathOption <- getMetadataField identifier "math"
  path <- getResourceFilePath
  let writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = case fmap (map toLower) mathOption of
              Just "mathml" -> MathML
              _ | enableMathml -> MathML
              _ -> MathJax ""
          }
  pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions (internalLinkTransform path)

keepAlphaNum :: Char -> Char
keepAlphaNum x
  | isAlphaNum x = x
  | otherwise = ' '

clean :: T.Text -> T.Text
clean =
  T.map keepAlphaNum . T.replace "'" "" . T.replace "&" "and"

toSlug :: T.Text -> T.Text
toSlug =
  T.intercalate (T.singleton '-') . T.words . T.toLower . clean

titleRoute :: Metadata -> Routes
titleRoute =
  constRoute . fileNameFromMetadata

fileNameFromMetadata :: Metadata -> String
fileNameFromMetadata =
  toFileName . getTitleFromMeta

toFileName :: String -> String
toFileName = T.unpack . (`T.append` ".html") . toSlug . T.pack

getTitleFromMeta :: Metadata -> String
getTitleFromMeta m =
  fromMaybe "no title" $ lookupString "slug" m <|> lookupString "title" m

fileNameFromStrings :: [(Maybe String, String)] -> String
fileNameFromStrings list =
  toFileName
    $ fromMaybe "no title"
    $ (findAttribute "slug" list)
      <|> (findAttribute "title" list)

findAttribute :: String -> [(Maybe String, String)] -> Maybe String
findAttribute a list = fmap snd $ find myFind list
  where
    myFind :: (Maybe String, String) -> Bool
    myFind (x, _) = case x of
      Just a -> True
      _ -> False

-- | Internal link transformation
internalLinkTransform :: FilePath -> Pandoc -> Compiler Pandoc
internalLinkTransform route = walkM (internalLinkTransform' route)

internalLinkTransform' :: FilePath -> Inline -> Compiler Inline
internalLinkTransform' route orig@(Link attr inl (url, title)) = do
  res <- getIdentifierURLCompiler route (fromFilePath $ T.unpack url)
  return $ go res
  where
    go :: (Store.Result String) -> Inline
    go (Store.Found newUrl) = Link attr inl (T.pack newUrl, title)
    go _ = orig
internalLinkTransform' _ x = return x

getIdentifierURLCompiler :: FilePath -> Identifier -> Compiler (Store.Result String)
getIdentifierURLCompiler path identifier = unsafeCompiler $ do
  let newPath = joinPath $ drop 1 $ splitPath $ normalise $ takeDirectory path </> (toFilePath identifier)
  store <- Store.new False $ storeDirectory config
  Store.get store [newPath]

setIdentifierURLCompiler :: Identifier -> String -> Compiler ()
setIdentifierURLCompiler identifier url = unsafeCompiler $ do
  store <- Store.new False $ storeDirectory config
  Store.set store [normalise $ toFilePath identifier] url

-- | From org get metadatas.
orgCompiler :: Item String -> Compiler (Item String)
orgCompiler i = do
  identifier <- getUnderlying
  withItemBody (go identifier) i
  where
    go :: Identifier -> String -> Compiler String
    go identifier s =
      let filename = fileNameFromStrings $ orgMetadata s
          result = (metadatasToStr . orgMetadata) s ++ s
       in fmap (\_ -> result) $ setIdentifierURLCompiler identifier $ toUrl filename

-- ( \s -> fmap ()
--     unsafeCompiler $ setIdentifierURL identifier $ toUrl filename
--     return (metadatasToStr . orgMetadata) s ++ s
-- )
-- i

orgMetadata :: String -> [(Maybe String, String)]
orgMetadata = map (format . clean) . getFirstLines . lines
  where
    getFirstLines = takeWhile (/= "")
    clean :: String -> String
    clean = concat . splitOn "#+"
    format :: String -> (Maybe String, String)
    format s = case splitOn ":" s of
      [] -> (Nothing, "")
      x : xs -> (Just (map toLower x), intercalate ":" xs)

metadatasToStr :: [(Maybe String, String)] -> String
metadatasToStr list =
  let dashes = "----------"
      ss = map (\(x, y) -> intercalate ":" $ (maybeToList x) ++ [y]) list
   in unlines $ ([dashes] ++) $ (++ [dashes]) ss

tempRoute :: Routes
tempRoute = customRoute tempRoute'
  where
    tempRoute' i = ".." </> "_temp" </> takeDirectory p </> takeFileName p
      where
        p = toFilePath i
