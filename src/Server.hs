{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Server.Internal
import Server.Utils (mainHtml)
import Application.Types
import Templates
import Blog (handleBlogPosts)
import UrlPath
import Web.Page.Lucid

import Web.Scotty.Trans
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Pandoc
import Text.Blaze.Renderer.Text (renderMarkup)

import Data.Monoid
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

mainHandler :: ( MonadIO m
               , MonadReader Env m
               , Functor m
               ) => ScottyT LT.Text m ()
mainHandler = do
  pr <- envPrefix <$> lift ask
  homePage <- (readMarkdown def) <$> (liftIO $ readFile $ pr <> "pages/home.md")
  ( get "/" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Home") $ toHtmlRaw $
        renderMarkup $ writeHtml def {writerHtml5 = True} homePage )
  handleBlogPosts
