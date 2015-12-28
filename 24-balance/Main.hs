-- stack runghc --verbosity error --package parallel

{-# LANGUAGE ScopedTypeVariables #-}

import Control.DeepSeq (NFData, force)
import Control.Monad (guard)
import Control.Parallel (par, pseq)
import qualified Data.List as L
import Data.Ord (comparing)
import System.IO (hFlush, stdout)

interleave = L.concat . L.transpose

withEach :: a -> [[a]] -> [[[a]]]
withEach x [] = []
withEach x (xs:xss) = ((x:xs):xss) : map (xs:) (withEach x xss)

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = let ysss = partitions xs
  in interleave [map ([x]:) ysss, concatMap (withEach x) ysss]

partitionInto :: NFData a => Int -> [a] -> [[[a]]]
partitionInto 0 _ = []
partitionInto n xs | n == length xs = [map (:[]) xs]
partitionInto n (x:xs) =
  let ysss = map ([x]:) $ partitionInto (n-1) xs
      zsss = concatMap (withEach x) $ partitionInto n xs
  -- in force ysss `par` (force zsss `pseq` ysss ++ zsss)
  in ysss ++ zsss

allEqual [] = True
allEqual (x:xs) = all (==x) xs

-- 2-partitions, passenger-first, s.t. passenger has required total
candidates :: Int -> [Int] -> [[[Int]]]
candidates total sizes = do
  [passenger, rest] <- map (L.sortOn length) $ partitionInto 2 sizes
  guard $ sum passenger == total
  return [passenger, rest]

canPartition 1 total sizes = sum sizes == total
canPartition n total sizes = or $ do
  [part, rest] <- partitionInto 2 sizes
  guard $ sum part == total
  return $ canPartition (n-1) total rest

-- passengers from qualified partitions
passengers :: Int -> [Int] -> [[Int]]
passengers n sizes = do
  let total = sum sizes `div` n
  [passenger, rest] <- L.sortOn (length . head) $ candidates total sizes
  guard $ canPartition (n-1) total rest
  return passenger

-- Best quantum entanglement for `n` groups over `sizes`
best :: Int -> [Int] -> Int
best n sizes =
  let ps = passengers n sizes
      shortest = length $ head ps
  in minimum $ map product $ takeWhile ((==shortest) . length) ps

main :: IO ()
main = do
  sizes :: [Int] <- map read . lines <$> getContents
  print $ best 3 sizes
  print $ best 4 sizes
