-- stack runghc --verbosity error --package cryptohash --package bytestring --package base16-bytestring

import Control.Monad (forM_)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16

type Message = B.ByteString
type Digest = B.ByteString

startsWithZeroes :: Int -> Digest -> Bool
startsWithZeroes n = B.isPrefixOf (BC.pack $ replicate n '0') . B16.encode

message :: String -> Int -> Message
message key n = BC.pack $ key ++ show n

test :: String -> Int -> Int -> Bool
test k n = startsWithZeroes n . MD5.hash . message k

main :: IO ()
main = do
  let key = "ckczppom"
  forM_ [5,6] $ \i -> print $ until (test key i) (+1) 1
