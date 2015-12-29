-- stack runghc --verbosity error

import Control.Monad (forM_)
import Data.List (isInfixOf, group)

preds1 =
  [ (>=3) . length . filter (`elem` "aeiou")
  , (>=2) . maximum . map length . group
  , not . or . zipWith ($) (map isInfixOf ["ab", "cd", "pq", "xy"]) . repeat
  ]

hasPairs :: String -> Bool
hasPairs (x:y:xs) = [x,y] `isInfixOf` xs || hasPairs (y:xs)
hasPairs _ = False

hasSplitRepeat :: String -> Bool
hasSplitRepeat (x:y:z:xs) = x == z || hasSplitRepeat (y:z:xs)
hasSplitRepeat _ = False

preds2 = [hasPairs, hasSplitRepeat]

matchesAll :: [a -> Bool] -> a -> Bool
matchesAll preds = and . zipWith ($) preds . repeat

main :: IO ()
main = do
  ls <- lines <$> getContents
  forM_ [preds1, preds2] $ \preds ->
    print $ length $ filter (matchesAll preds) ls
  -- print $ hasPairs "qjhvhtzxzqqjkmpb"

