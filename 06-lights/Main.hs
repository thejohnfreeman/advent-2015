{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Primitive (PrimState, PrimMonad)
-- import qualified Data.Array.Repa as R
-- import Data.Array.Repa ((:.)(..), Z(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser $ emptyDef
  { P.reservedNames = ["through", "turn", "toggle"] }

reserved   = P.reserved lexer
lexeme     = P.lexeme lexer
natural    = P.natural lexer
identifier = P.identifier lexer

type Point = (Int,Int)

data Rectangle = Rectangle Point Point
  deriving (Eq, Show)

data Instruction
  = Turn Bool Rectangle
  | Toggle Rectangle
  deriving (Eq, Show)

point :: Parser Point
point = do
  x <- natural
  lexeme $ char ','
  y <- natural
  return (fromInteger x, fromInteger y)

rect :: Parser Rectangle
rect = do
  tl <- point
  reserved "through"
  br <- point
  return $ Rectangle tl br

turn :: Parser Instruction
turn = do
  reserved "turn"
  state <- identifier
  r <- rect
  return $ Turn (state == "on") r

toggle :: Parser Instruction
toggle = do
  reserved "toggle"
  r <- rect
  return $ Toggle r

instruction :: Parser Instruction
instruction = turn <|> toggle

instructions :: Parser [Instruction]
instructions = instruction `manyTill` eof

{-
type Grid r = R.Array r R.DIM2 Bool

darkGrid :: Int -> Int -> Grid R.U
darkGrid m n = R.fromListUnboxed (Z :. m :. n) $ repeat False

isWithin :: R.DIM2 -> Rectangle -> Bool
isWithin (Z :. i :. j) (Rectangle (l,t) (r,b)) =
  l <= i && i <= r && b <= j && j <= t

exec1 :: R.Source r Bool => Grid r -> Instruction -> Grid R.D
exec1 g (Turn st r) = R.traverse g id $ \prev i ->
                      if i `isWithin` r then st else prev i
exec1 g (Toggle r) = R.traverse g id $ \prev i ->
                      if i `isWithin` r then not (prev i) else prev i

numLit :: (R.Source r Bool, R.Source r Int) => Grid r -> Int
numLit = R.sumAllS . R.map (\b -> if b then 1 else 0)
-}

type Grid m a = V.Vector (VUM.MVector (PrimState m) a)

modify :: (PrimMonad m, VUM.Unbox a) => (a -> a) -> VUM.MVector (PrimState m) a -> m ()
modify f v = forM_ [0..VUM.length v - 1] $ \i -> do
  x <- VUM.read v i
  VUM.write v i $ f x

-- exec1 :: PrimMonad m => Grid m Bool -> Instruction -> m ()
-- exec1 grid (Turn v (Rectangle (l,b) (r,t))) =
--   V.forM_ (V.slice b (t-b+1) grid) $ \row ->
--     VUM.set (VUM.slice l (r-l+1) row) v
-- exec1 grid (Toggle (Rectangle (l,b) (r,t))) =
--   V.forM_ (V.slice b (t-b+1) grid) $ \row ->
--     modify not $ VUM.slice l (r-l+1) row
exec1 :: PrimMonad m => Grid m Int -> Instruction -> m ()
exec1 grid (Turn v (Rectangle (l,b) (r,t))) =
  V.forM_ (V.slice b (t-b+1) grid) $ \row ->
    modify (if v then (+1) else max 0 . subtract 1) $ VUM.slice l (r-l+1) row
exec1 grid (Toggle (Rectangle (l,b) (r,t))) =
  V.forM_ (V.slice b (t-b+1) grid) $ \row ->
    modify (+2) $ VUM.slice l (r-l+1) row

-- countLit :: V.Vector (VU.Vector Bool) -> Int
-- countLit = V.sum . V.map (VU.length . VU.filter id)

sumBrightness :: V.Vector (VU.Vector Int) -> Int
sumBrightness = V.sum . V.map VU.sum

main :: IO ()
main = do
  input <- getContents
  let insts = either (error . show) id $ parse instructions "<stdin>" input
  grid <- V.replicateM 1000 $ VUM.replicate 1000 0
  mapM_ (exec1 grid) insts
  grid' <- V.mapM VU.freeze grid
  print $ sumBrightness grid'
