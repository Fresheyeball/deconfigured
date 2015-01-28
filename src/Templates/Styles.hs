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
  background: #b67a52;
}

#nav {
  padding-top: 1rem;
  background: #d9a084;
}

#content {
  background: #e6c0ad;
  padding-top: 1rem;
  padding-bottom: 1rem;
}

footer {
  padding: 1rem;
  text-align: right;
}

blockquote {
  border-left: 1rem solid #c49986;
  background: #d1ad9e;
  padding-bottom: 0.5rem;
}

blockquote code {
  background: #dcbeb6;
  border: 1px solid #e4cdc6;
}

blockquote p:last-child {
  margin-bottom: 0;
}

blockquote p {
  color: #555;
}

hr {
  border: 0.5rem solid #d1ad9e;
}

h1 {
  margin-left: 2rem;
}

h2 {
  text-decoration: underline;
}

h3 {
  text-align: right;
  padding-right: 2rem;
  margin-right: 1rem;
  margin-left: 3rem;
  font-family: "Liberation Serif", serif;
  font-style: italic;
  box-shadow: inset 0 -1.25rem 0 #D1AD9E;
}

a, a:link, a:active, a:visited {
  color: #95502d;
  text-decoration: none;
}

a:hover {
  color: #cd6942;
  text-decoration: none;
}

pre {
  margin-bottom: 1rem;
}

code {
  white-space: pre;
  width: 100%;
  background: #e3cbbe;
  border-radius: 0.25rem;
  border: 1px solid #eed4ca;
}

pre code {
  display: inline-block;
  border-radius: 0.5rem;
  border: 1px solid #d9a995;
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

.date {
  color: #434343;
}

  |]
