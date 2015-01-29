{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Templates where

import Templates.Styles (mainStyle, mediaQueries)
import Templates.Scripts (googleAnalytics, cornerRound)

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
mainPage :: ( Monad m
            ) => WebPage (HtmlT (AbsoluteUrlT T.Text m) ()) T.Text
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
    , deploy JavaScript cornerRound
    ]
  styles' = renderMarkup cdnStyles
        --  <> renderMarkup localStyles
         <> renderMarkup inlineStyles
  cdnStyles :: Monad m => HostedMarkupM (HtmlT m ())
  cdnStyles = mconcat
    [ deploy Css
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/css/normalize.min.css" :: T.Text)
    , deploy Css
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/css/foundation.min.css" :: T.Text)
    , deploy Css
        ("//cdnjs.cloudflare.com/ajax/libs/octicons/2.1.2/octicons.min.css" :: T.Text)
    ]
  inlineStyles :: Monad m => InlineMarkupM (HtmlT m ())
  inlineStyles = mconcat
    [ deploy Css mainStyle
    , deploy Css mediaQueries
    ]
  -- localStyles :: ( Monad m
  --                , Url T.Text w
  --                ) => LocalMarkupM (HtmlT (w T.Text m) ())
  -- localStyles = mconcat
  --   [ deploy Css (plainUrl "css/cardinal/css/main.css" :: UrlString T.Text)
  --   ]
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
mainTemplate page content = template page $ mconcat
  [ header_ [class_ "row"] $ mconcat
      [ a_ [href_ "/"] $ h1_ [id_ "logo"] "DeConfigured"
      , p_ [id_ "subtitle"] "Athan Clark's Blog"
      ]
  , div_ [class_ "row"] $ mconcat
      [ div_ [ class_ "columns small-12 medium-3 large-2"
             , id_ "nav"
             ] $ ul_ [] $ mconcat $
                  map (li_ [])
                    [ a_ [href_ "/"] "Home"
                    , (a_ [href_ "/blog"] "Blog")
                   <> (ul_ [] $ mconcat $
                        map (li_ [])
                          [ a_ [href_ "/blog/dag"] "DAG"
                          -- ,
                          ])
                    , a_ [href_ "/bookshelf"] "Bookshelf"
                    , (a_ [href_ "/contact"] "Contact")
                   <> (ul_ [] $ mconcat $
                        map (li_ [])
                          [ a_ [href_ "/cv"] "C.V."
                          , a_ [href_ "http://www.linkedin.com/pub/athan-clark/56/612/557"] $ mconcat
                            [ img_ [ id_ "linkedin"
                                   , src_ "/images/linkedin.png"
                                   ]
                            , toHtmlRaw "&nbsp;LinkedIn"
                            ]
                          , a_ [href_ "http://www.github.com/athanclark"] $ mconcat
                            [ span_ [ class_ "octicon octicon-mark-github"
                                    ] ""
                            , toHtmlRaw "&nbsp;GitHub"
                            ]
                          ])
                    ]
      , div_ [ class_ "columns small-12 medium-9 large-10"
             , id_ "content"
             ] content
      ]
  , div_ [class_ "row"] $ footer_ $
      p_ [] $ toHtmlRaw "Copyright &copy; 2015 Athan Clark, All Rights Reserved"
  ]
