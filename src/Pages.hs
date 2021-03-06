{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Pages where

import Lucid
import Lucid.Base

import Data.Monoid

cvPage :: Monad m => HtmlT m ()
cvPage = mconcat
  [ h1_ [] "Curriculum Vitæ"
  , a_ [ style_ "position:absolute;right:2rem;top:2rem"
       , href_ "/public-athanclark.pdf" ] "pdf version"
  , h2_ [] "Profile"
  , p_ [] $ mconcat
      [ "Self educated software engineer and computer scientist. Well versed - object oriented, "
      , "functional, and domain-specific. Practical GNU/Linux and UNIX admin - can adopt any environment. "
      , "Result-oriented, well-disciplined and tactical - skilled in complexity management and formal testing. "
      , "Proficient in type safety and proofs."
      ]
  , p_ [] $ mconcat
      [ "Involved in advanced web development research projects - WebSockets, SVG, Animation, etc. "
      , "Well versed multi-paradigm developer, with deep understandings of Functional and Object Oriented design."
      ]
  , h2_ [] "Published Projects"
  , table_ [class_ "wide"] $ mconcat
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
              , "Trivially generalizes Html based on the style of how it's content is sourced - a simpler method for working with Html."
              ]
          ,
              [ strong_ [] "UrlPath"
              , "Method writing deployable urls - where the hostname is encoded in context, aiding in the port of a web application from one host to another."
              ]
          ]
      ]
  , h2_ [] "Technical Skills"
  , h3_ [] "Languages"
  , table_ [class_ "wide"] $ mconcat
      [ thead_ [] $ tr_ [] $ mconcat $ map (th_ [])
          [ "Language"
          , "Description"
          ]
      , tbody_ [] $ mconcat $ map (tr_ [] . mconcat . map (td_ []))
          [
              [ strong_ [] "Haskell"
              , "Haskell is the most advanced practical programming language as-of-date. I use it for my web servers, compilers, type systems, etc. - it's fast, clean, stable, and popular."
              ]
          ,
              [ strong_ [] "JavaScript"
              , "Node.js and it's surrounding projects have come a long way in the last 5 years - people are now paying more attention to clean composition and ideal code. I use JavaSript and Bower for my front-end development."
              ]
          ,
              [ strong_ [] "Ruby"
              , "Rails is everywhere, unfortunately, so it's important to know the trade."
              ]
          ,
              [ strong_ [] "PHP"
              , "This is another universal eye-sore that's important to know. I used to use PHP for my main web server development using Laravel, composer, and PEAR."
              ]
          ,
              [ strong_ [] $ toHtmlRaw "Python & C/C++"
              , "I have studied CPU architecture and low-level C programming (and assembly) for a while now. I only know enough to provide useful additions, not for orchestrating a codebase."
              ]
          ]
      ]
  , h3_ [] "Principles"
  , mconcat $ map (\(x,y) -> oneQuart x y)
      [ ("Assurance", "Unit and Automated Testing with Property Establishment gives us some pseudo-certainty, but mastering Type Systems is how we get absolute correctness. Isolated Sandboxing and Reproducible Development also go hand-in-hand with this concern, Nix is a wonderful example.")
      , ("Performance", "Profiling is leveraged where Complexity and Asymptotic Analysis are just too difficult.")
      , ("Availability", "Browser-Bug Compensation and Evasion, Backward and Forward Compatibility, and Search Engine Optimization are key features for any website.")
      , (toHtmlRaw "Clarity &amp; Design", "The things we create are ideas, materialized in code. But, this materialization is surjective, not bijective! It's critically important to be clear, and compensate where you're not.")
      ]
  , h3_ [] "Development"
  , mconcat $ map (\(x,y) -> oneQuart x y)
      [ ("Administration", toHtmlRaw "Installation & Configuration, Kernel Compilation, Hardware Optimization &amp; Troubleshooting")
      , ("Tools", toHtmlRaw "Make &amp; Autotools; Gulp, Grunt, Bower, NPM, PEAR, Composer & Cabal; Git, Darcs, Cvs; Vim, Atom")
      , (toHtmlRaw "Networking &amp; Basic Security", toHtmlRaw "Routing &amp; Switching, SELinux, File Permissions")
      , ("Distributions", "Ubuntu/Debian, Red Hat/Fedora, Gentoo, NixOS, Linux From Scratch, FreeBSD")
      ]
  , h3_ [] "Active Research"
  , mconcat $ map (\(x,y) -> oneQuart x y)
      [ ("Ambiguous Lambda Calculus", "New form of lambda calculus with Natural-number based function arity. It is designed for a flexible method of function application - perfect for arbitrary text templates.")
      , ("PureScript", "Purely-functional programming language that compiles to JavaScript - it is Haskell based, with some potential improvements (particularly in it's effect system).")
      , ("Cryptol", "Well-typed bit calculator featuring an interesting variant of dependent type theroy, where types depend on bit depth size.")
      , ("Agda", "A beautifully semantic dependently-typed programming language and theorem prover, similar to Haskell.")
      ]
  , h2_ [] "Past Work"
  , table_ [class_ "wide"] $ mconcat
        [ thead_ [] $ tr_ [] $ mconcat $ map (th_ [])
            [ "Company"
            , "Accomplishments"
            ]
        , tbody_ [] $ mconcat $ map (tr_ [] . mconcat . map (td_ []))
            [
                [ strong_ [] "American Financing"
                , "Upgraded their legacy PHP and Ruby codebase to a unified Haskell web application - a mirror image of the previous (with semantic HTML improvements), while compensating for historic urls for proper redirection. Completed in 2 weeks."
                ]
            ,
                [ strong_ [] "Peerless Network"
                , "Developed command-line applications for the Network Operations Center to maximize help-desk throughput."
                ]
            ,
                [ strong_ [] "Kifaru Outdoors"
                , "Emergency web application - an iTunes clone for iPhones, but accessible from the web - to allow remote control of the song playing from the dance floor. Completed in 5 days."
                ]
            ]
        ]
  , hr_ []
  , blockquote_ [] $ mconcat
      [ "Please "
      , a_ [href_ "/contact"] "contact me"
      , " if you need references!"
      ]
  ]
  where
    oneQuart :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
    oneQuart x y = div_ [class_ "row"] $ mconcat
                      [ div_ [class_ "columns small-3 large-3"] $ strong_ [] x
                      , div_ [class_ "columns small-9 large-9"] y
                      ]
