-- stack runghc --verbosity error

import Data.Char (ord, chr)
import qualified Data.List as L
import qualified Data.Set as S

incChar :: Char -> Char
incChar c = chr $ ord c + 1

incString :: String -> String
incString = reverse . incString' . reverse
  where
    incString' [] = []
    incString' ('z':cs) = 'a' : incString' cs
    incString' (c:cs) = incChar c : cs

hasStraight :: Int -> String -> Bool
hasStraight n s | length s < n = False
hasStraight n s@(h:t) = take n (iterate incChar h) `L.isPrefixOf` s || hasStraight n t

hasNoneOf :: String -> String -> Bool
hasNoneOf cs s = and $ L.notElem <$> cs <*> [s]

hasDoubles :: Int -> String -> Bool
hasDoubles n = (>=n) . S.size . S.fromList . filter ((>=2) . length) . L.group

incPassword :: String -> String
incPassword = until isGood incString . incString
  where isGood s = hasStraight 3 s && hasNoneOf "iol" s && hasDoubles 2 s

main :: IO ()
-- main = putStrLn $ incPassword "abcdefgh"
-- main = putStrLn $ incPassword "ghijklmn"
main = mapM_ putStrLn $ take 2 $ drop 1 $ iterate incPassword "vzbxkghb"
