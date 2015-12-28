-- stack runghc --verbosity error

{-# LANGUAGE LambdaCase #-}

import qualified Data.List as L
import qualified Data.Set as S

type Position = (Int, Int)
type Direction = Char

move1 :: Position -> Direction -> Position
move1 (x,y) = \case
  '<' -> (x-1,y)
  '>' -> (x+1,y)
  'v' -> (x,y-1)
  '^' -> (x,y+1)
  _   -> (x,y)

locsVisited :: Position -> [Direction] -> S.Set Position
locsVisited p ds = S.fromList $ scanl move1 p ds

numVisited :: Position -> [Direction] -> Int
numVisited p ds = S.size $ locsVisited p ds

-- Which is better?
roundRobin :: Int -> [a] -> [[a]]
roundRobin n xs = L.transpose $ L.unfoldr f xs
  where f xs = if null xs then Nothing else Just $ splitAt n xs
-- roundRobin n xs =
--   let (hs, ts) = splitAt n xs
--   in zipWith (:) hs (roundRobin n ts ++ repeat [])

numVisitedWithRobo :: Position -> [Direction] -> Int
numVisitedWithRobo p = S.size . S.unions . map (locsVisited p) . roundRobin 2

main :: IO ()
main = do
  dirs <- getContents
  print $ numVisited (0,0) dirs
  print $ numVisitedWithRobo (0,0) dirs
