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

@darkMain: #95502d;
@otherDarkMain: #c49986;
@otherMain: #d1ad9e;

@font-face {
  font-family: "deftone";
  src: url("/fonts/deftone_stylus.ttf");
}

@font-face {
  font-family: "norton";
  src: url("/fonts/NORTON.TTF");
}

@font-face {
  font-family: "fontleroy";
  src: url("/fonts/LittleLordFontleroyNF.otf");
}

body {
  background: #ac6546;
}

#nav {
  padding-top: 1rem;
  background: #d9a084;
}

#nav ul {
  list-style: none;
  margin-left: 0;
}

#nav ul ul {
  list-style: none;
  margin-left: 0;
  padding-left: 0.5rem;
}

#nav ul li {
  line-height: 1.2em;
  color: #95502D;
}

#nav ul ul li:before {
  font-size: 1.2em;
  content: "├ ";
}

#nav ul ul li:last-child:before {
  font-size: 1.2em;
  content: "└ ";
}

#content {
  background: #e6c0ad;
  padding: 1.5rem;
}

footer {
  padding: 1rem;
  text-align: right;
}

header {
  vertical-align: bottom;
  height: 9rem;
  position: relative;
}

#logo {
  color: #fff;
  text-shadow: 0.2rem 0.2rem #{darkMain};
}

#subtitle {
  -webkit-font-smoothing: antialiased;
  font-smoothing: antialiased;
  font-family: "fontleroy", "Liberation Sans", sans;
  color: #ddd;
  font-size: 2rem;
  font-style: italic;
  text-shadow: 0.1rem 0.1rem #{darkMain};
  margin-left: 6rem;
  margin-bottom: 0;
  position: absolute;
  bottom: 0;
}

#linkedin {
  width: 15px;
  height: 15px;
  vertical-align: -1px;
  border: 0;
}

.octicons {
  vertical-align: 1px;
}

blockquote {
  border-left: 1rem solid #{otherDarkMain};
  background: #{otherMain};
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
  padding-left: 0;
  color: #555;
}

hr {
  border: 0.5rem solid #{otherMain};
}

h1 {
  margin-left: 2rem;
  -webkit-font-smoothing: antialiased;
  font-smoothing: antialiased;
  font-family: "deftone", sans;
}

h2 {
  -webkit-font-smoothing: antialiased;
  font-smoothing: antialiased;
  font-family: "norton", sans;
  box-shadow: inset 0 -0.3rem #222;
  width: auto;
  display: inline-block;
}

h3 {
  text-align: right;
  padding-right: 2rem;
  margin-right: 1rem;
  margin-left: 3rem;
  font-style: italic;
  box-shadow: inset 0 -1.25rem 0 #D1AD9E;
}

h5 {
  font-weight: bold;
}

a, a:link, a:active, a:visited {
  color: #95502d;
  text-decoration: none;
  transition-property: color;
  transition-duration: 0.3s;

}

a:hover {
  color: #cd6942;
  text-decoration: none;
}

pre {
  margin-bottom: 1rem;
}

p:last-child {
  margin-bottom: 0;
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

mediaQueries = LT.concat
  [ "@media print {"
  , "  html {"
  , "    margin: 25mm;"
  , "  }"
  , "}"
  , "@media only screen and (min-width: 64.063em) {"
  , "  #nav {"
  , "    border-radius: 0.5rem 0 0 0.5rem;"
  , "  }"
  , "  #content {"
  , "    border-radius: 0 0.5rem 0.5rem 0.5rem;"
  , "  }"
  , "}"
  ]
