import Data.List (isInfixOf)

-- There has to be a better way...
longestRepeat :: Eq a => [a] -> Int
longestRepeat [] = 0
longestRepeat (x:xs) = longestRepeat' 1 1 x xs
  where longestRepeat' n i _ [] = max n i
        longestRepeat' n i x (y:ys)
          | x == y = longestRepeat' n (i+1) x ys
          | otherwise = longestRepeat' (max n i) 1 y ys

repeatsPairNonOverlapping :: String -> Bool
repeatsPairNonOverlapping (x:y:xs) =
  [x,y] `isInfixOf` xs || repeatsPairNonOverlapping (y:xs)
repeatsPairNonOverlapping _ = False

hasSplitRepeat :: String -> Bool
hasSplitRepeat (x:y:z:xs) = x == z || hasSplitRepeat (y:z:xs)
hasSplitRepeat _ = False

isNiceLine :: String -> Bool
isNiceLine = and . zipWith ($) [f, g, h] . repeat
  -- where f = (>=3) . length . filter (`elem` "aeiou")
  --       g = (>=2) . longestRepeat
  --       h = not . or . zipWith ($) (map isInfixOf ["ab", "cd", "pq", "xy"]) . repeat
  where f = repeatsPairNonOverlapping
        g = hasSplitRepeat
        h = const True

main :: IO ()
main = getContents >>= print . length . filter id . map isNiceLine . lines
-- main = print $ repeatsPairNonOverlapping "qjhvhtzxzqqjkmpb"

