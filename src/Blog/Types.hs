{-# LANGUAGE DeriveFunctor #-}

module Blog.Types where

import Text.Pandoc

-- | A simple data type representing a post.
data Post a = Post { slug :: String
                   , meta :: Meta
                   , content :: a }
  deriving (Show, Eq, Functor)
