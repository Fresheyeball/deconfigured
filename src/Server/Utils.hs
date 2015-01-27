{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Utils where

import Application.Types
import UrlPath

import Web.Scotty.Trans
import Lucid.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader.Class


mainHtml :: ( MonadReader Env m
            , MonadIO m
            , Functor m
            ) => HtmlT (AbsoluteUrlT T.Text Identity) ()
              -> ActionT LT.Text m ()
mainHtml content = do
  (root :: T.Text) <- (T.pack . envHostname) <$> lift ask
  html $ runIdentity $ (runUrlReader $ renderTextT content) root
