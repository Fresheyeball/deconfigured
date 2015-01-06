{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Styles where

import Text.Lucius
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH

mainStyle :: LT.Text
mainStyle = renderCss $ mainStyle' undefined
  where
  mainStyle' = [lucius|

body {
  background: cyan;
}

  |]
