{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Templates where

import UrlPath
import Web.Page.Lucid
import Lucid.Base
import qualified Data.Text as T

import Data.Default
import Data.Functor.Identity

mainPage :: Monad m =>
            WebPage (HtmlT m ()) T.Text
mainPage = def

mainTemplate :: WebPage (HtmlT (AbsoluteUrlT T.Text Identity) ()) T.Text
             -> HtmlT (AbsoluteUrlT T.Text Identity) ()
             -> HtmlT (AbsoluteUrlT T.Text Identity) ()
mainTemplate page content =
  template page content
