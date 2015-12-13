-- stack runghc --verbosity error

import Control.Monad
import Data.List

delta :: Char -> Int
delta '(' = 1
delta ')' = -1
delta _ = 0

firstBasement :: String -> Maybe Int
firstBasement = findIndex (<0) . scanl (+) 0 . map delta

main :: IO ()
-- main = getContents >>= print . sum . map delta
main = getContents >>= putStrLn . maybe "nothing" show . firstBasement
