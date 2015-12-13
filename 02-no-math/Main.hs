-- stack runghc --verbosity error --package split

import Data.List
import Data.List.Split

data Box = Box [Int]

parse :: String -> [Box]
parse = map pBox . lines
  where pBox = Box . map read . splitOn "x"

paper1 :: Box -> Int
paper1 (Box dims) = surfaceArea dims + product (take 2 $ sort dims)
  where surfaceArea [l,w,h] = 2*(l*w + w*h + l*h)

ribbon1 :: Box -> Int
ribbon1 (Box dims) = 2 * sum (take 2 $ sort dims) + product dims

main :: IO ()
main = do
 boxes <- parse <$> getContents
 print $ sum . map paper1 $ boxes
 print $ sum . map ribbon1 $ boxes
-- main = print $ paper1 $ Box [1,1,10]
-- main = print $ paper1 $ Box [2,3,4]
-- main = print $ ribbon1 $ Box [1,1,10]
-- main = print $ ribbon1 $ Box [2,3,4]
