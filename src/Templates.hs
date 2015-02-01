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
mainPage = (def :: Monad m => WebPage (HtmlT (AbsoluteUrlT T.Text m) ()) T.Text)
               { pageTitle = "DeConfigured"
               , bodyScripts = bodyScripts'
               , styles = styles'
               , afterStylesScripts = afterStylesScripts'
               , metaVars = mconcat
                              [ metaVars
                                  (def :: Monad m => WebPage (HtmlT (AbsoluteUrlT T.Text m) ()) T.Text)
                              , mconcat $ map mkAppleIcon
                                  [ "57x57"
                                  , "60x60"
                                  , "72x72"
                                  , "76x76"
                                  , "120x120"
                                  , "114x114"
                                  , "144x144"
                                  , "152x152"
                                  , "180x180"
                                  ]
                              , link_ [ rel_ "icon"
                                      , type_ "image/png"
                                      , href_ "/favicon-32x32.png"
                                      , sizes_ "32x32" ]
                              , link_ [ rel_ "icon"
                                      , type_ "image/png"
                                      , href_ "/android-chrome-192x192.png"
                                      , sizes_ "192x192" ]
                              , link_ [ rel_ "icon"
                                      , type_ "image/png"
                                      , href_ "/favicon-96x96.png"
                                      , sizes_ "96x96" ]
                              , link_ [ rel_ "icon"
                                      , type_ "image/png"
                                      , href_ "/favicon-16x16.png"
                                      , sizes_ "16x16" ]
                              , link_ [ rel_ "manifest"
                                      , href_ "/android-chrome-manifest.json" ]
                              , meta_ [ name_ "msapplication-TileColor"
                                      , content_ "#ac6546" ]
                              , meta_ [ name_ "msapplication-TileImage"
                                      , content_ "/mstile-144x144.png" ]
                              , meta_ [ name_ "theme-color"
                                      , content_ "#95502D" ]
                              , meta_ [ makeAttribute "property" "og:image"
                                      , content_ "http://deconfigured.com/apple-touch-icon-precomposed.png" ]
                              ]
               , favicon = mempty
               }
  where
  -- mkAppleIcon :: Monad m => T.Text -> HtmlT m ()
  mkAppleIcon x = link_ [ rel_ "apple-touch-icon"
                        , sizes_ x
                        , href_ $ "/apple-touch-icon-" <> x <> ".png" ]
  bodyScripts' = renderMarkup cdnBodyScripts
              <> renderMarkup inlineBodyScripts
  cdnBodyScripts :: Monad m => HostedMarkupM (HtmlT m ())
  cdnBodyScripts = mconcat
    [ deploy JavaScript
        ("//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js" :: T.Text)
    , deploy JavaScript
        ("//cdnjs.cloudflare.com/ajax/libs/foundation/5.5.0/js/foundation.min.js" :: T.Text)
    , deploy JavaScript ("//u.heatmap.it/log.js" :: T.Text)
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
  , div_ [class_ "row"] $ do
      div_ [ class_ "small-12 medium-3 large-2 columns"
           , style_ "margin:0;padding:0"
           ] $ do
                  div_ [id_ "nav", class_ "columns"] $ ul_ $ mconcat $
                    map (li_ [])
                      [ a_ [href_ "/"] "Home"
                      , (a_ [href_ "/blog"] "Blog")
                     <> (ul_ [] $ mconcat $
                          map (li_ [])
                            [ a_ [href_ "/blog/dag"] "DAG"
                            , mconcat [ a_ [href_ "/blog/atlc"] "ATλC"
                                      , "-"
                                      , a_ [href_ "/blog/atlc2"] "2"
                                      , "-"
                                      , a_ [href_ "/blog/atlc3"] "3"
                                      ]
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
                  -- google ad
                  script_ [ async_ mempty
                          , src_ "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js" ] ""
                  ins_ [ class_ "adsbygoogle"
                       , style_ "display:inline-block;width:100%;height:125px;"
                       , data_ "ad-client" "ca-pub-4103661590917429"
                       , data_ "ad-slot" "2322436590" ] ""
                  (script_ []
                    ("(adsbygoogle = window.adsbygoogle || []).push({});" :: T.Text)
                    :: HtmlT (AbsoluteUrlT T.Text Identity) () )
                  div_ [class_ "selfieAd"] $ do
                    em_ [] "...psst..."
                    h4_ [] "Wanna Keep me Alive?"
                    a_ [ href_ "/contact"
                       , class_ "button round"
                       ] "Hire Me!"
      div_ [ class_ "columns small-12 medium-9 large-10"
           , id_ "content"
           ] content
  , div_ [class_ "row"] $ footer_ $
      p_ [] $ toHtmlRaw "Copyright &copy; 2015 Athan Clark, All Rights Reserved"
  ]
