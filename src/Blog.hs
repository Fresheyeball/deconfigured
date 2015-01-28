{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Blog.Types

import Templates
import Application.Types
import Server.Utils (mainHtml)
import UrlPath

import Lucid
import Lucid.Base
import Web.Scotty.Trans
import Text.Pandoc
import Text.Highlighting.Kate.Styles (espresso)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Directory (getDirectoryContents)
import Text.Blaze.Renderer.Text (renderMarkup)

import Data.Monoid
import Data.Functor.Identity
import Data.Default
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader.Class
import System.Time
import System.Time.ParseDate (parseCalendarTime)
import System.Locale

handleBlogPosts :: ( MonadIO m
                   , MonadReader Env m
                   , Functor m
                   ) => ScottyT LT.Text m ()
handleBlogPosts = do
  let
    postTitle :: Meta -> T.Text
    postTitle = T.pack . inlineToString . docTitle
    postDate :: Meta -> String
    postDate = formatCalendarTime defaultTimeLocale "%A, %B %e, %Y"
             . fromJust
             . parseCalendarTime defaultTimeLocale "%m/%d/%Y"
             . inlineToString
             . docDate

  pr <- envPrefix <$> lift ask
  postFiles <- (filter (\x -> x /= "." && x /= "..")) <$>
                  (liftIO $ getDirectoryContents $ pr <> "blog")
  postMetas <- mapM (\f -> postFileMeta $ pr <> "blog/" <> f) postFiles
  -- liftIO $ print postMetas

  let
    makeSummary :: Meta -> FilePath -> HtmlT (AbsoluteUrlT T.Text Identity) ()
    makeSummary meta file =
      let slug = takeWhile (/= '.') file in
      li_ [] $ mconcat
        [ a_ [href_ $ "/blog/" <> (T.pack slug)] $
            toHtmlRaw $ postTitle meta
        , " - "
        , em_ [class_ "date"] $ toHtmlRaw $ postDate meta
        ]

    summaries = ul_ [] $ mconcat $ map (uncurry makeSummary) $
                  sortMetaMap $ zip postMetas postFiles

  -- Handle the index
  ( get "/blog" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Blog") $
        summaries
    )

  mapM_ handleBlogPost postFiles
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
          renderedContents = renderMarkup $ writeHtml
                              def { writerHighlight = True
                                  , writerHighlightStyle = espresso
                                  , writerHtml5 = True
                                  , writerTableOfContents = True } $ parsedContents

      get (capture $ "/blog/" <> (takeWhile (/= '.') fileName)) $
        mainHtml $ mainTemplate
                    (mainPage `appendTitle` "Blog" `appendTitle` T.pack title) $
                      toHtmlRaw renderedContents

postFileMeta :: MonadIO m => FilePath -> m Meta
postFileMeta file = do
  contents <- liftIO $ readFile file
  return $ getMeta $ readMarkdown def contents
  where
    getMeta (Pandoc m _) = m
