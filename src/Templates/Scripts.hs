{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Scripts where

import Text.Julius
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH

googleAnalytics :: LT.Text
googleAnalytics = renderJavascript $ goog' undefined
  where
  goog' = [js|

  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-58406146-1', 'auto');
  ga('send', 'pageview');

  |]
