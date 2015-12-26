-- stack runghc --verbosity error

import qualified Data.List as L
import Data.Ord (comparing)
import Data.Traversable (forM)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser $ emptyDef
  { P.reservedNames = ["capacity", "durability", "flavor", "texture", "calories"]
  , P.reservedOpNames = [":", ","]
  }

identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
integer = fromInteger <$> P.integer lexer

type Ingredient = [Int]
_capacity   = (!! 0)
_durability = (!! 1)
_flavor     = (!! 2)
_texture    = (!! 3)
_calories   = (!! 4)

pIngredient :: Parser (String, Ingredient)
pIngredient = do
  name <- identifier
  reservedOp ":"
  reserved "capacity"
  capacity <- integer
  reservedOp ","
  reserved "durability"
  durability <- integer
  reservedOp ","
  reserved "flavor"
  flavor <- integer
  reservedOp ","
  reserved "texture"
  texture <- integer
  reservedOp ","
  reserved "calories"
  calories <- integer
  return (name, [capacity, durability, flavor, texture, calories])

type Ingredients = [(String, Ingredient)]

pIngredients :: Parser Ingredients
pIngredients = pIngredient `manyTill` eof

-- Recipe is a list of weights.
type Recipe = [Int]

-- recipes length total
-- Return all recipes of given length whose weights sum to given total.
recipes :: Int -> Int -> [Recipe]
recipes 1 tot = [[tot]]
recipes n tot = do
  w <- [0..tot]
  ws <- recipes (n - 1) (tot - w)
  return $ w:ws

mix :: Ingredients -> Recipe -> Ingredient
mix is ws = L.foldl1' (zipWith (+)) $ zipWith (map . (*)) ws (map snd is)

score1 :: Ingredient -> Int
score1 = product . map (max 0) . take 4

main :: IO ()
main = do
  input <- getContents
  let is = either (error . show) id $ parse pIngredients "<stdin>" input
  -- print $ score1 is [44,56]
  -- print $ L.maximumBy (comparing $ score1 is) $ recipes (length is) 100
  let ms = map (mix is) $ recipes (length is) 100
  print $ L.maximum $ map score1 ms
  print $ L.maximum $ map score1 $ filter ((== 500) . (!! 4)) ms

