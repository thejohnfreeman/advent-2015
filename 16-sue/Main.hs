-- stack runghc --verbosity error --package parsec

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))

import Text.Parsec
import Text.Parsec.String

nat :: (Read a, Num a) => Parser a
nat = read <$> many1 digit

type Item = (String, Int)
type Items = [Item]

data Sue = Sue
  { _num :: Int
  , _items :: Items
  }
  deriving (Show)

pItem :: Parser Item
pItem = do
  key <- many1 letter
  string ": "
  value <- nat
  return (key, value)

pItems :: Parser Items
pItems = pItem `sepBy` string ", "

pSue :: Parser Sue
pSue = do
  string "Sue "
  n <- nat
  string ": "
  items <- pItems
  endOfLine
  return $ Sue n items

pSues :: Parser [Sue]
pSues = pSue `manyTill` eof

has :: (Hashable k, Eq k, Eq v) => M.HashMap k v -> (k, v) -> Bool
has m (k, v) = (m ! k) == v

compounds = M.fromList
            [ ("children", 3)
            , ("cats", 7)
            , ("samoyeds", 2)
            , ("pomeranians", 3)
            , ("akitas", 0)
            , ("vizslas", 0)
            , ("goldfish", 5)
            , ("trees", 3)
            , ("cars", 2)
            , ("perfumes", 1)
            ]

isSue :: Sue -> Bool
isSue = all (compounds `has`) . _items

isRealSue :: Sue -> Bool
isRealSue = all (matches compounds) . _items
  where matches cs (k, v)
          | k `elem` ["cats", "trees"] = (cs ! k) < v
          | k `elem` ["pomeranians", "goldfish"] = (cs ! k) > v
          | otherwise = (cs ! k) == v

main :: IO ()
main = do
  input <- getContents
  let sues = either (error . show) id $ parse pSues "<stdin>" input
  -- case filter isSue sues of
  case filter isRealSue sues of
    [sue] -> print $ _num sue
    [] -> putStrLn "Sue not found"
    sues -> putStrLn $ "too many Sues: " ++ show sues

