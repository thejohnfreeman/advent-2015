-- stack runghc --verbosity error --package aeson

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.List (unfoldr, tails)
import Data.Maybe
import qualified Data.Scientific as SCI

readMany :: Read a => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

rsum :: JS.Value -> Integer
rsum (JS.Number n) = either (const 0) id $ SCI.floatingOrInteger n
rsum (JS.Object o) = sum $ map rsum $ M.elems o
rsum (JS.Array a) = sum $ map rsum $ V.toList a
rsum _ = 0

withoutRed :: JS.Value -> JS.Value
withoutRed (JS.Object o) | JS.String "red" `elem` M.elems o = JS.Null
withoutRed (JS.Object o) = JS.Object $ M.map withoutRed o
withoutRed (JS.Array a) = JS.Array $ V.map withoutRed a
withoutRed v = v

main :: IO ()
-- main = getContents >>= print . sum . readMany
main = getContents >>= print . rsum . withoutRed . fromJust . JS.decode . BSC.pack
