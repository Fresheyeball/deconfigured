{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Templates where

import Templates.Styles (mainStyle)
import Templates.Scripts (googleAnalytics)

import UrlPath
import Web.Page.Lucid
import Data.Markup
import Lucid
import Lucid.Base
import qualified Data.Text as T

import Data.Monoid
import Data.Default
import Data.Functor.Identity

-- | This is the default main template.
mainPage :: Monad m =>
            WebPage (HtmlT m ()) T.Text
mainPage = def { pageTitle = "DeConfigured"
               , bodyScripts = bodyScripts'
               , styles = styles'
               , afterStylesScripts = afterStylesScripts'
               }
  where
  bodyScripts' = renderMarkup cdnBodyScripts
              <> renderMarkup inlineBodyScripts
  cdnBodyScripts :: Monad m => HostedMarkupM (HtmlT m ())
  cdnBodyScripts = mconcat
    [ deploy JavaScript
        ("//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js" :: T.Text)
    , deploy JavaScript
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/js/foundation.min.js" :: T.Text)
    ]
  inlineBodyScripts :: Monad m => InlineMarkupM (HtmlT m ())
  inlineBodyScripts = mconcat
    [ deploy JavaScript
        ("$(document).foundation();" :: T.Text)
    , deploy JavaScript googleAnalytics
    ]
  styles' = renderMarkup cdnStyles
         <> renderMarkup inlineStyles
  cdnStyles :: Monad m => HostedMarkupM (HtmlT m ())
  cdnStyles = mconcat
    [ deploy Css
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/css/normalize.min.css" :: T.Text)
    , deploy Css
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/css/foundation.min.css" :: T.Text)
    ]
  inlineStyles :: Monad m => InlineMarkupM (HtmlT m ())
  inlineStyles = mconcat
    [ deploy Css mainStyle
    ]
  afterStylesScripts' = renderMarkup cdnStylesScripts
  cdnStylesScripts :: Monad m => HostedMarkupM (HtmlT m ())
  cdnStylesScripts = mconcat
    [ deploy JavaScript
        ("//cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.min.js" :: T.Text)
    ]

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
mainTemplate page content = template page $
  div_ [class_ "row"] $ mconcat
    [ div_ [class_ "columns small-6 large-3"] ""
    , div_ [class_ "columns small-6 large-3"] ""
    , div_ [class_ "columns small-6 large-6"] content
    ]
