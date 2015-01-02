{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Templates where

import UrlPath
import Web.Page.Lucid
import Data.Markup
import Lucid.Base
import qualified Data.Text as T

import Data.Monoid
import Data.Default
import Data.Functor.Identity

mainPage :: Monad m =>
            WebPage (HtmlT m ()) T.Text
mainPage = def { pageTitle = "DeConfigured"
               , bodyScripts = bodyScripts'
               }
  where
  bodyScripts' = renderMarkup bodyScriptsMarkup
  bodyScriptsMarkup :: Monad m => HostedMarkupM (HtmlT m ())
  bodyScriptsMarkup = do
    deploy JavaScript
      ("//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js" :: T.Text)

appendTitle :: Monad m =>
               WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = pageTitle page
                              <> " → " <> x
                          }

mainTemplate :: WebPage (HtmlT (AbsoluteUrlT T.Text Identity) ()) T.Text
             -> HtmlT (AbsoluteUrlT T.Text Identity) ()
             -> HtmlT (AbsoluteUrlT T.Text Identity) ()
mainTemplate page content =
  template page content
