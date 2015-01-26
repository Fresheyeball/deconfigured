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
                     -> [FilePath]
                     -> ScottyT LT.Text m ()
handleBlogPosts mainHtml posts = do
  pr <- envPrefix <$> lift ask

  -- Handle the index
  let postMetas :: (MonadIO q, Functor q) => q [Meta]
      postMetas = mapM (\f -> postFileMeta $ pr <> "blog/" <> f) posts
      postTitle :: Meta -> T.Text
      postTitle = T.pack . inlineToString . docTitle
      summaries :: (MonadIO q, Functor q) => q [HtmlT (AbsoluteUrlT T.Text Identity) ()]
      summaries = fmap (map (\p -> p_ [] $ toHtmlRaw $ postTitle p)) postMetas

  ( get "/blog" $ do
    summs <- summaries
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Blog") $
        mconcat summs
    )

  mapM_ (\x -> handleBlogPost $  x) posts
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
                                  , writerHighlightStyle = espresso} $ parsedContents

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
