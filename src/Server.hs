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
import Lucid
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
  makeMdPage "pages/home.md" "/" "Home"
  makeMdPage "pages/bookshelf.md" "/bookshelf" "Bookshelf"
  makeMdPage "pages/contact.md" "/contact" "Contact"
  cvPage <- (readMarkdown def) <$> (liftIO $ readFile $ pr <> "pages/cv.md")
  ( get "/cv" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Curriculum Vitæ") $ h1_ [] "Curriculum Vitæ"
        <> (toHtmlRaw $ renderMarkup $ writeHtml def {writerHtml5 = True} cvPage) )
  handleBlogPosts

makeMdPage file url title = do
  pr <- envPrefix <$> lift ask
  page <- (readMarkdown def) <$> (liftIO $ readFile $ pr <> file)
  ( get (capture url) $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` title) $ toHtmlRaw $
        renderMarkup $ writeHtml def {writerHtml5 = True} page )
