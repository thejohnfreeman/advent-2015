-- stack runghc --verbosity error --package text --package text-icu

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.ICU as U
import qualified Data.Text.IO as TIO

preds1 =
  [ (>=3) . length . U.findAll (U.regex [] "a|e|i|o|u")
  , isJust . U.find (U.regex [] "(.)\\1")
  , isNothing . U.find (U.regex [] "ab|cd|pq|xy")
  ]

preds2 = [ isJust . U.find (U.regex [] p) | p <- ["(..).*\\1", "(.).\\1"] ]

matchesAll :: [a -> Bool] -> a -> Bool
matchesAll preds = and . zipWith ($) preds . repeat

main :: IO ()
main = do
  ls <- T.lines <$> TIO.getContents
  forM_ [preds1, preds2] $ \preds ->
    print $ length $ filter (matchesAll preds) ls
