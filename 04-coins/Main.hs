-- stack runghc --verbosity error --package cryptohash --package bytestring

import Control.Monad (forM_)
import Crypto.Hash (Digest, MD5, hash)
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)

md5 :: String -> Digest MD5
md5 = hash . BC.pack

test :: String -> Int -> Int -> Bool
test key n = isPrefixOf (replicate n '0') . show . md5 . (key++) . show

main :: IO ()
main = forM_ [5,6] $ \n -> print $ until (test "ckczppom" n) (+1) 1
