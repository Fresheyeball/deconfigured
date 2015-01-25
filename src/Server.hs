{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Server.Internal
import Application.Types
import Templates
import UrlPath
import Web.Page.Lucid

import Web.Scotty.Trans
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Directory (doesFileExist)
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
  ( get "/" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Home") "aww yea" )
  ( get "/blog" $ do
    mainHtml $ mainTemplate
      (mainPage `appendTitle` "Blog") "Blog" )
  ( get "/blog/:slug" $ do
    (slug :: LT.Text) <- param "slug" -- FIXME: This catches empty, too!
    pr <- envPrefix <$> lift ask
    exists <- liftIO $ doesFileExist $ pr <> "blog/" <> LT.unpack slug <> ".md"
    if exists then mainHtml $ mainTemplate
                    (mainPage `appendTitle` "Blog") $ toHtmlRaw slug
              else status notFound404 )

mainHtml :: ( MonadReader Env reader
            , MonadIO reader
            , Functor reader
            ) => HtmlT (AbsoluteUrlT T.Text Identity) ()
              -> ActionT LT.Text reader ()
mainHtml content = do
  (root :: T.Text) <- (T.pack . envHostname) <$> lift ask
  html $ runIdentity $ (runUrlReader $ renderTextT content) root
