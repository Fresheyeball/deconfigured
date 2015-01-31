{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Pages where

import Lucid
import Lucid.Base

import Data.Monoid

cvPage :: Monad m => HtmlT m ()
cvPage = mconcat
  [ h1_ [] "Curriculum Vitæ"
  , h2_ [] "Profile"
  , p_ [] $ mconcat
      [ "Self educated software engineer and technology professional. Well versed - object oriented, "
      , "functional, and domain-specific. Practical GNU/Linux and UNIX admin - can adopt any environment. "
      , "Result-oriented, well-disciplined and tactical - skilled in complexity management and formal testing. "
      , "Proficient in type safety and proofs."
      ]
  , p_ [] $ mconcat
      [ "Involved in advanced web development research projects - WebSockets, SVG, Animation, etc. "
      , "Well versed multi-paradigm developer, with deep understandings of Functional and Object Oriented design."
      ]
  , h2_ [] "Published Projects"
  , table_ [] $ mconcat
      [ thead_ [] $ tr_ [] $ mconcat $ map (th_ [])
          [ "Name"
          , "Description"
          ]
      , tbody_ [] $ mconcat $ map (tr_ [] . mconcat . map (td_ []))
          [
              [ strong_ [] "DAG"
              , "Pioneered a new api for producing compile-time directed acyclic graphs in pure haskell. It uses an advanced form of Type-Level programming to facilitate acyclic safety."
              ]
          ,
              [ strong_ [] "Markup"
              , "Generalizes Html based on the style of how it's content is sourced - a simpler method for working with Html."
              ]
          ,
              [ strong_ [] "UrlPath"
              , "Semantic method writing deployable urls, alleviating the web developer from plaintext urls."
              ]
          ]
      ]
  , h2_ [] "Technical Skills"
  , h3_ [] "Languages"
  , h4_ [] $ toHtmlRaw "Functional &amp; Pure"
  , div_ [class_ "row"] $ mconcat
      [ div_ [class_ "columns small-12 large-6"] $ ul_ [] $ mconcat $ map (li_ [])
          [ "Haskell"
          , "PureScript"
          ]
      , div_ [class_ "columns small-12 large-6"] $ ul_ [] $ mconcat $ map (li_ [])
          [ "Idris, Agda"
          , "Cryptol"
          ]
      ]
  , h4_ [] $ toHtmlRaw "Imperative &amp; Object-Oriented"
  , div_ [class_ "row"] $ mconcat
      [ div_ [class_ "columns small-12 large-6"] $ ul_ [] $ mconcat $ map (li_ [])
          [ "C/C++"
          , "PHP, Python"
          ]
      , div_ [class_ "columns small-12 large-6"] $ ul_ [] $ mconcat $ map (li_ [])
          [ toHtmlRaw "ECMAScript 3/5/6, JavaScipt &nbsp; node.js"
          , "Ruby and Rails"
          ]
      ]
  , h3_ [] "Principles"
  , mconcat $ map (\(x,y) -> oneQuart x y)
      [ ("Assurance", "Unit and Automated Testing, Property Establishment, Type Systems, Isolated Sandboxing, Reproducible Development")
      , ("Performance", "Profiling, Complexity and Asymptotic Analysis, Mathematics")
      , ("Availability", "Browser-Bug Compensation and Evasion, Backward and Forward Compatibility, Search Engine Optimization, High Availability, Semantic Modeling")
      , (toHtmlRaw "Clarity &amp; Design", "Elegant Code, Linting Tools, Design Patterns (MVC, Gang of Four, Refactoring), Natural Expression")
      ]
  , h3_ [] "Development"
  , mconcat $ map (\(x,y) -> oneQuart x y)
      [ ("Administration", toHtmlRaw "Installation & Configuration, Kernel Compilation, Hardware Optimization &amp; Troubleshooting")
      , ("Tools", toHtmlRaw "Make &amp; Autotools; Gulp, Grunt, Bower, NPM, PEAR, Composer & Cabal; Git, Darcs, Cvs; Vim")
      , (toHtmlRaw "Networking &amp; Basic Security", toHtmlRaw "Routing &amp; Switching, SELinux, File Permissions")
      , ("Distributions", "Ubuntu/Debian, Red Hat/Fedora, Gentoo, NixOS, Linux From Scratch, FreeBSD")
      ]
  , hr_ []
  , blockquote_ [] $ mconcat
      [ "Please "
      , a_ [href_ "/contact"] "contact me"
      , " if you need references or hard copies!"
      ]
  ]
  where
    oneQuart :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
    oneQuart x y = div_ [class_ "row"] $ mconcat
                      [ div_ [class_ "columns small-3 large-3"] $ strong_ [] x
                      , div_ [class_ "columns small-9 large-9"] y
                      ]
