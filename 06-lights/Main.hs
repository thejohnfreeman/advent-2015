{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (void)
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..), Z(..))
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

main :: IO ()
main = getContents >>= putStrLn . either show (show . const (numLit $ darkGrid 1000 1000)) . parse instructions "<stdin>"

