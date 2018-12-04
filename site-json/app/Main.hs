{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable
import qualified Data.Aeson as A
import Slick
import Data.List
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Filter.IncludeCode (includeCode)
import Control.Monad
import GHC.Generics (Generic)
import Development.Shake.Classes
import Control.Lens
import Data.Aeson.Lens
import Data.Char (isSpace)
import Data.Text.Lens
import Data.Bifunctor
import Data.Either.Combinators


markdownReaderWithSnippets :: PandocReader T.Text
markdownReaderWithSnippets txt = do
  pdoc <- readMarkdown markdownOptions txt
  liftIO . includeCodeTransform $ pdoc

includeCodeTransform :: Pandoc -> IO Pandoc
includeCodeTransform = walkM (includeCode (Just (Format "html5")))

newtype ArticleReq = ArticleReq String
  deriving (Show, Eq, Hashable, Binary, NFData)

data Article = Article
  { what :: T.Text
  , why :: T.Text
  , title :: T.Text
  , section :: T.Text
  , content :: T.Text
  , slug :: T.Text
  , url :: T.Text
  , sortKey :: T.Text
  } deriving Generic

instance A.FromJSON Article
instance A.ToJSON Article

slugify :: T.Text -> T.Text
slugify =
  T.filter (not . isSpace)
    . T.intercalate "-"
    . T.words
    . T.replace "&" "and"
    . T.replace "+" "and"
    . T.filter
        (not . (`elem` ("@#=><%+&$!^*?()[]{}`./\\'\"~|" :: String)))
    . T.toLower
    . T.strip

loadArticle :: ArticleReq -> Action Article
loadArticle (ArticleReq srcPath) = do
  articleContents <- readFile' srcPath
  articleData     <-
    loadUsing markdownReaderWithSnippets
              (writeHtml5String html5Options)
    . T.pack
    $ articleContents
  either fail return (toArticle articleData)

buildURL :: A.Value -> Maybe T.Text
buildURL val = T.pack <$> do
  slugSection <-
    val ^? key "section" . _String . to slugify . unpacked
  slugTitle <- val ^? key "title" . _String . to slugify . unpacked
  return ("/articles/" </> slugSection </> slugTitle)


toArticle :: A.Value -> Either String Article
toArticle val =
  Article
    <$> maybeToRight "missing 'what'"  (val ^? key "what" . _String)
    <*> maybeToRight "missing 'why'"   (val ^? key "why" . _String)
    <*> maybeToRight "missing 'title'" (val ^? key "title" . _String)
    <*> maybeToRight "missing 'section'"
                     (val ^? key "section" . _String)
    <*> maybeToRight "missing 'content'"
                     (val ^? key "content" . _String)
    <*> maybeToRight "missing 'title'"
                     (val ^? key "title" . _String . to slugify)
    <*> maybeToRight "missing 'url'" (buildURL val)
    <*> maybeToRight "missing 'sortKey'"
                     (val ^? key "sortKey" . _String)

loadAllArticles :: (ArticleReq -> Action Article) -> Action [Article]
loadAllArticles articleCache = do
  articleSrcPaths <- getDirectoryFiles "./articles/markdown/"
                                       ["//*.md"]
  traverse
    (articleCache . ArticleReq . ("./articles/markdown/" </>))
    articleSrcPaths

main :: IO ()
main = shakeArgs shakeOptions $ do
  articleCache <- jsonCache' loadArticle
  "json" ~> need ["site/src/data/articles.json"]
  "site/src/data/articles.json" %> \out -> do
    allArticles <- loadAllArticles articleCache
    liftIO $ A.encodeFile out allArticles
