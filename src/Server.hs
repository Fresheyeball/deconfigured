{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
      (mainPage `appendTitle` "Home") "aww yee" )

mainHtml :: ( MonadReader Env reader
            , MonadIO reader
            , Functor reader
            ) => HtmlT (AbsoluteUrlT T.Text Identity) ()
              -> ActionT LT.Text reader ()
mainHtml content = do
  (root :: T.Text) <- (T.pack . envHostname) <$> lift ask
  html $ runIdentity $ (runUrlReader $ renderTextT content) root
