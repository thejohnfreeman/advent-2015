-- stack runghc --verbosity error

import Data.Maybe (fromJust)
import Data.List (findIndex, scanl1)

delta :: Char -> Int
delta '(' = 1
delta ')' = -1
delta _ = 0

main :: IO ()
main = do
  ds <- map delta <$> getContents
  print $ sum ds
  print $ fromJust $ findIndex (<0) $ scanl1 (+) ds
