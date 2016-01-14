-- stack runghc --verbosity error --resolver lts-4.1 --package parsec --package vector

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import qualified Control.Monad.Primitive as P
import Data.Ix (range)
import qualified Data.Vector.Unboxed as UV
import qualified Grid as G
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

type Grid m a = G.Grid (Int, Int) (UV.MVector (P.PrimState m)) a

modifyRange g f lb rt = mapM_ (G.modify g f) $ range (lb,rt)

exec1 :: P.PrimMonad m => Grid m Bool -> Instruction -> m ()
exec1 g (Turn v (Rectangle lb rt)) = modifyRange g (const v) lb rt
exec1 g (Toggle (Rectangle lb rt)) = modifyRange g not lb rt

exec2 :: P.PrimMonad m => Grid m Int -> Instruction -> m ()
exec2 g (Turn v (Rectangle lb rt))
  = modifyRange g (if v then (+1) else max 0 . subtract 1) lb rt
exec2 g (Toggle (Rectangle lb rt)) = modifyRange g (+2) lb rt

main :: IO ()
main = do
  input <- getContents
  let insts = either (error . show) id $ parse instructions "<stdin>" input
  grid <- G.replicate (1000,1000) False
  mapM_ (exec1 grid) insts
  grid' <- G.freeze grid :: IO (G.Grid (Int, Int) UV.Vector Bool)
  print $ G.length . G.filter id $ grid'
  grid <- G.replicate (1000,1000) 0
  mapM_ (exec2 grid) insts
  grid' <- G.freeze grid :: IO (G.Grid (Int, Int) UV.Vector Int)
  print $ G.sum grid'
