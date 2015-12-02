import Control.Monad
import Data.List

delta :: Char -> Int
delta '(' = 1
delta ')' = -1
delta _ = 0

firstBasement :: String -> Maybe Int
firstBasement = findIndex (<0) . scanl (+) 0 . map delta

(>>==) = flip liftM
infixl 7 >>==

main :: IO ()
-- main = getContents >>== sum . map delta >>= print
main = getContents >>== firstBasement >>== maybe "nothing" show >>= putStrLn
