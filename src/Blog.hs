{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Templates
import Application.Types
import UrlPath

import Lucid.Base
import Web.Scotty.Trans
import Text.Pandoc
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
  mapM_ (\x -> handleBlogPost $  x) posts
  where
    handleBlogPost fileName = do
      pr       <- envPrefix <$> lift ask
      contents <- liftIO $ readFile $ pr <> "blog/" <> fileName
      let renderedContents = writeHtmlString def $ readMarkdown def contents
      get (capture $ "/blog/" <> (takeWhile (/= '.') fileName)) $
        mainHtml $ mainTemplate
                    (mainPage `appendTitle` "Blog") $ toHtmlRaw renderedContents
