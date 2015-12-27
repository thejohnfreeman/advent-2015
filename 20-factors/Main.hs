-- stack runghc --verbosity error

import Control.Monad (forM_)
import qualified Data.List as L

interleave :: [[a]] -> [a]
interleave = L.concat . L.transpose

divides :: Integral a => a -> a -> Bool
divides k n = (n `mod` k) == 0

-- least prime factor
lpf :: Integral a => a -> a
lpf = npf 2

-- next prime factor
npf :: Integral a => a -> a -> a
npf k n | k `divides` n = k
        | k ^ 2 > n = n
        | otherwise = npf (k+1) n

-- prime factors
factors :: Integral a => a -> [a]
factors 1 = [1]
factors n = let k = lpf n in k : factors (n `div` k)

divisors :: Integral a => a -> [a]
divisors = L.nub . map product . powerset . factors

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = interleave [xss, map (x:) xss] where xss = powerset xs

numPresents :: Integral a => a -> a
numPresents = (10*) . sum . divisors

numPresents' :: Integral a => a -> a
numPresents' n = (11*) $ sum $ filter ((>=n) . (*50)) $ divisors n

main :: IO ()
main = do
  -- forM_ [1..9] $ print . numPresents
  print $ head $ dropWhile ((<36000000) . numPresents) [800000..]
  print $ head $ dropWhile ((<36000000) . numPresents') [700000..]
