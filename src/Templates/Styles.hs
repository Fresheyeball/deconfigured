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
  border: 1px solid gray;
}

code {
  white-space: pre;
  width: 100%;
}

pre code {
  display: inline-block;
}

table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0;
  padding: 0;
  vertical-align: baseline;
  border: none;
}
table.sourceCode {
  width: 100%;
  line-height: 100%;
}
td.lineNumbers {
  text-align: right;
  padding-right: 4px;
  padding-left: 4px;
  color: #aaaaaa;
  border-right: 1px solid #aaaaaa;
}
td.sourceCode {
  padding-left: 5px;
}
code > span.kw { color: #007020; font-weight: bold; }
code > span.dt { color: #902000; }
code > span.dv { color: #40a070; }
code > span.bn { color: #40a070; }
code > span.fl { color: #40a070; }
code > span.ch { color: #4070a0; }
code > span.st { color: #4070a0; }
code > span.co { color: #60a0b0; font-style: italic; }
code > span.ot { color: #007020; }
code > span.al { color: #ff0000; font-weight: bold; }
code > span.fu { color: #06287e; }
code > span.er { color: #ff0000; font-weight: bold; }

  |]
