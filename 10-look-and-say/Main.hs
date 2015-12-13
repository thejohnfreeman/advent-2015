-- stack runghc --verbosity error

import qualified Data.List as L

lookAndSay :: String -> String
lookAndSay = concatMap las . L.group
  where las g = show (length g) ++ [head g]

main :: IO ()
main = do
  let input = "1113222113"
  let outputs = iterate lookAndSay input
  print $ length $ outputs !! 40
  print $ length $ outputs !! 50
