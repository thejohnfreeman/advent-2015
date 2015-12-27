-- stack runghc --verbosity error

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List as L
import Data.Ord (comparing)

fits :: Int -> [Int] -> [[Int]]
fits eggnog containers = filter ((==eggnog) . sum) $ L.subsequences containers

main :: IO ()
main = do
  sizes :: [Int] <- map read . lines <$> getContents
  let combos = fits 150 sizes
  print $ length combos
  let ns = map length combos
  let n = minimum ns
  print $ length $ filter (==n) ns

