import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

data Box = Box [Int]

pBox :: Parser Box
pBox = Box <$> int `sepBy1` char 'x' <* char '\n'

pBoxes :: Parser [Box]
pBoxes = pBox `manyTill` eof

paper1 :: Box -> Int
paper1 (Box dims) = surfaceArea dims + product (take 2 $ sort dims)
  where surfaceArea [l,w,h] = 2*(l*w + w*h + l*h)
        surfaceArea _ = undefined

paper :: [Box] -> Int
paper = sum . map paper1

ribbon1 :: Box -> Int
ribbon1 (Box dims) = 2 * sum (take 2 $ sort dims) + product dims

ribbon :: [Box] -> Int
ribbon = sum . map ribbon1

(>>==) :: Functor f => f a -> (a -> b) -> f b
(>>==) = flip fmap

main :: IO ()
-- main = getContents >>== parse pBoxes "<stdin>" >>== either show (show . paper) >>= putStrLn
main = getContents >>== parse pBoxes "<stdin>" >>== either show (show . ribbon) >>= putStrLn
-- main = print $ paper1 $ Box [1,1,10]
-- main = print $ paper1 $ Box [2,3,4]
-- main = print $ ribbon1 $ Box [1,1,10]
-- main = print $ ribbon1 $ Box [2,3,4]
