-- stack runghc --verbosity error --package split

{-# LANGUAGE TupleSections #-}

import Control.Monad.State.Lazy as STL
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.Split (onSublist, split, splitOn)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

type Replacement = (String, String)

identifier = many1 letter

pReplacement :: Parser Replacement
pReplacement = do
  f <- identifier
  string " => "
  t <- identifier
  endOfLine
  return (f, t)

pFile :: Parser ([Replacement], String)
pFile = do
  rs <- many pReplacement
  spaces
  m <- identifier
  return (rs, m)

replaceEach :: Eq a => ([a], [a]) -> [a] -> [[a]]
replaceEach (f, t) s = go t [] (split (onSublist f) s)
  where
    go t p (x:f:xs) = concat (p ++ (x:t:xs)) : go t (p ++ [x, f]) xs
    go _ _ _ = []

insertAll :: Ord a => S.Set a -> [a] -> S.Set a
insertAll = L.foldl' (flip S.insert)

-- Add to set all molecules reached from one replacement in given molecule
addAll :: String -> S.Set String -> Replacement -> S.Set String
addAll m ms r = insertAll ms $ replaceEach r m

type PathLengths = HM.HashMap String Int

pathsFrom :: [Replacement] -> String -> [(String, Int)]
pathsFrom rs s = go rs (S.singleton s) [(s, 0)]
  where
    go reps seen ((str, len):tips) =
      let strs' = filter (`S.notMember` seen) $ L.concatMap (`replaceEach` str) reps
          seen' = insertAll seen strs'
          tips' = map (,len+1) strs'
      in (str, len) : go reps seen' (tips ++ tips')

main :: IO ()
main = do
  input <- getContents
  let (exps, m) = either (error . show) id $ parse pFile "<stdin>" input
  -- print $ replaceEach "H" "HO" "HOH"
  print $ S.size $ L.foldl' (addAll m) S.empty exps
  let reds = map swap exps
  print $ snd $ head $ dropWhile ((/="e") . fst) $ pathsFrom reds m

