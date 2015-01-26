{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Templates
import Application.Types
import UrlPath

import Lucid
import Lucid.Base
import Web.Scotty.Trans
import Text.Pandoc
import Text.Highlighting.Kate.Styles (espresso)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Directory (getDirectoryContents)

import Data.Monoid
import Data.Functor.Identity
import Data.Default
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader.Class

handleBlogPosts :: ( MonadIO m
                   , MonadReader Env m
                   , Functor m
                   ) => ( HtmlT (AbsoluteUrlT T.Text Identity) ()
                       -> ActionT LT.Text m ()
                        )
                     -> ScottyT LT.Text m ()
handleBlogPosts mainHtml = do
  let
    postTitle :: Meta -> T.Text
    postTitle = T.pack . inlineToString . docTitle

  pr <- envPrefix <$> lift ask
  postFiles <- (filter (\x -> x /= "." && x /= "..")) <$>
                  (liftIO $ getDirectoryContents $ pr <> "blog")
  liftIO $ print postFiles
  postMetas <- mapM (\f -> postFileMeta $ pr <> "blog/" <> f) postFiles

  let
    makeSummary :: FilePath -> Meta -> HtmlT (AbsoluteUrlT T.Text Identity) ()
    makeSummary file meta =
      let slug = takeWhile (/= '.') file in
      li_ [] $
        a_ [href_ $ "/blog/" <> (T.pack slug)] $
          toHtmlRaw $ postTitle meta
    summaries = ul_ [] $ mconcat $ zipWith makeSummary postFiles postMetas

  -- Handle the index
  ( get "/blog" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Blog") $
        summaries
    )

  mapM_ (\x -> handleBlogPost $  x) postFiles
  where
    handleBlogPost fileName = do
      pr <- envPrefix <$> lift ask
      contents <- liftIO $ readFile $ pr <> "blog/" <> fileName
      let parsedContents = readMarkdown def contents
          getMeta (Pandoc m _) = m
          title :: String
          title   = inlineToString $ docTitle $ getMeta parsedContents
          authors :: String
          authors = concatMap inlineToString $
                      docAuthors $ getMeta parsedContents
          date :: String
          date    = inlineToString $ docDate $ getMeta parsedContents
          renderedContents = writeHtmlString
                              def { writerHighlight = True
                                  , writerHighlightStyle = espresso
                                  , writerHtml5 = True
                                  , writerTableOfContents = True } $ parsedContents

      get (capture $ "/blog/" <> (takeWhile (/= '.') fileName)) $
        mainHtml $ mainTemplate
                    (mainPage `appendTitle` "Blog" `appendTitle` T.pack title) $
                      toHtmlRaw renderedContents

inlineToString :: [Inline] -> String
inlineToString = foldl (\a x -> a <> inlineToString' x) ""
  where
    inlineToString' (Str s) = s
    inlineToString' (Space) = " "
    inlineToString' _ = ""

postFileMeta :: MonadIO m => FilePath -> m Meta
postFileMeta file = do
  contents <- liftIO $ readFile file
  return $ getMeta $ readMarkdown def contents
  where
    getMeta (Pandoc m _) = m
