{-# LANGUAGE OverloadedStrings #-}

module Server where

import Server.Internal
import UrlPath
import Web.Page.Lucid

import Web.Scotty.Trans
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Control.Monad.IO.Class

mainHandler :: MonadIO m => ScottyT LT.Text m ()
mainHandler = do
  ( get "/" $ do
    html "Awww yee" )
