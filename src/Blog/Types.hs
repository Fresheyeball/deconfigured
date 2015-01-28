{-# LANGUAGE DeriveFunctor #-}

module Blog.Types where

import Text.Pandoc
import Data.Monoid
import Data.Maybe (fromJust)
import System.Locale
import System.Time
import System.Time.ParseDate (parseCalendarTime)

-- | A simple data type representing a post.
data Post a = Post { slug :: String
                   , meta :: Meta
                   , content :: a }
  deriving (Show, Eq, Functor)

sortMetaMap :: [(Meta, FilePath)] -> [(Meta, FilePath)]
sortMetaMap zs = map twos $ foldl go [] zs
  where
  go :: [(CalendarTime, Meta, FilePath)]
     ->                (Meta, FilePath)
     -> [(CalendarTime, Meta, FilePath)]
  go [] (m,f) = [(parseDate m, m, f)]
  go xss@((n,m',f'):xs) (m,f) | n == parseDate m = (parseDate m, m, f) : xss
                              | n > parseDate m = (n,m',f') : go xs (m,f)
                              | n < parseDate m = -- fringe case TODO
                                                  (parseDate m, m, f) : xss

  twos (_,x,y) = (x,y)

  -- Expects MM/DD/YY
  parseDate :: Meta -> CalendarTime
  parseDate = fromJust . parseCalendarTime defaultTimeLocale "%D"
            . inlineToString . docDate

inlineToString :: [Inline] -> String
inlineToString = foldl (\a x -> a <> inlineToString' x) ""
  where
    inlineToString' (Str s) = s
    inlineToString' (Space) = " "
    inlineToString' _ = ""
