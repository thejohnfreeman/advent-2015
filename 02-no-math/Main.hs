-- stack runghc --verbosity error --package split

import qualified Data.List as L
import Data.List.Split (splitOn)

-- three dimensions, sorted from lowest to highest
type Box = [Int]

parse :: String -> [Box]
parse = map pBox . lines
  where pBox = L.sort . map read . splitOn "x"

paper1 :: Box -> Int
paper1 [x,y,z] = 2*(x*y + y*z + x*z) + x*y

ribbon1 :: Box -> Int
ribbon1 [x,y,z] = 2*(x+y) + x*y*z

main :: IO ()
main = do
  boxes <- parse <$> getContents
  print $ sum . map paper1 $ boxes
  print $ sum . map ribbon1 $ boxes
  -- print $ paper1 [1,1,10]
  -- print $ paper1 [2,3,4]
  -- print $ ribbon1 [1,1,10]
  -- print $ ribbon1 [2,3,4]
