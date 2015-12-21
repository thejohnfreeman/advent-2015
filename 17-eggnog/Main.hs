-- stack runghc --verbosity error

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List as L
import Data.Ord (comparing)

interleave :: [[a]] -> [a]
interleave = L.concat . L.transpose

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = interleave [xss, map (x:) xss] where xss = powerset xs

fits :: Int -> [Int] -> [[Int]]
fits eggnog containers = filter ((==eggnog) . sum) $ powerset containers

main :: IO ()
main = do
  sizes :: [Int] <- map read . lines <$> getContents
  let combos = fits 150 sizes
  print $ length combos
  let ns = map length combos
  let n = minimum ns
  print $ length $ filter (==n) ns

