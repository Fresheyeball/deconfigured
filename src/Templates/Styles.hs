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
  background: white;
}

pre {
  background-color: #FDF6E3;
  padding: 0.1rem;
}

.sourceCode .kw { color: #268BD2; }
.sourceCode .dt { color: #268BD2; }

.sourceCode .dv, .sourceCode .bn, .sourceCode .fl { color: #D33682; }
.sourceCode .ch { color: #DC322F; }
.sourceCode .st { color: #2AA198; }
.sourceCode .co { color: #93A1A1; }
.sourceCode .ot { color: #A57800; }
.sourceCode .al { color: #CB4B16; font-weight: bold; }
.sourceCode .fu { color: #268BD2; }
.sourceCode .re { }
.sourceCode .er { color: #D30102; font-weight: bold; }

.sourceCode span {
  margin: 0;
  padding: 0;
}

  |]
