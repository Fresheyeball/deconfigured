{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Server.Internal
import Application.Types
import Templates
import Blog
import UrlPath
import Web.Page.Lucid

import Web.Scotty.Trans
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Directory (getDirectoryContents)
import Network.HTTP.Types (notFound404)

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
  pr    <- envPrefix <$> lift ask
  postFiles <- liftIO $ getDirectoryContents $ pr <> "blog/"
  ( get "/" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Home") "aww yea" )
  ( get "/blog" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Blog") "Blog" )
  handleBlogPosts mainHtml $ drop 2 postFiles -- remove "." and ".."

mainHtml :: ( MonadReader Env reader
            , MonadIO reader
            , Functor reader
            ) => HtmlT (AbsoluteUrlT T.Text Identity) ()
              -> ActionT LT.Text reader ()
mainHtml content = do
  (root :: T.Text) <- (T.pack . envHostname) <$> lift ask
  html $ runIdentity $ (runUrlReader $ renderTextT content) root
