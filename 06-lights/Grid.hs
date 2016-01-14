{-# LANGUAGE FlexibleInstances #-}

module Grid where

import qualified Control.Monad.Primitive as P
import Data.Bifunctor (first)
import qualified Data.List as L
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MGV
import Prelude hiding (map)

data Grid s v a = Grid
  { shape :: s
  , buffer :: v a
  } deriving Show

-- A shape is also an index.
class Shape a where
  origin :: a
  size :: a -> Int
  fromShape :: a -> a -> Int
  toShape :: Int -> a -> a

instance Shape Int where
  origin = 0
  size = id
  fromShape = const
  toShape = const

instance Shape (Int, Int) where
  origin = (0, 0)
  -- rows, columns
  size (r, c) = r * c
  (i, j) `fromShape` (_, c) = i*c + j
  ind `toShape` (_, c) = ind `divMod` c

instance Shape (Int, Int, Int) where
  origin = (0, 0, 0)
  -- layers, rows, columns
  size (l, r, c) = l * r * c
  (i, j, k) `fromShape` (_, r, c) = i*r*c + j*c + k
  ind `toShape` (_, r, c) =
    let (i, ind') = ind `divMod` (r*c)
        (j, k) = ind' `divMod` c
    in (i, j, k)

fromList :: GV.Vector v a => s -> [a] -> Grid s v a
fromList sh xs = Grid sh $ GV.fromList xs

fromStream :: (Shape s, GV.Vector v a) => s -> [a] -> Grid s v a
fromStream sh xs = Grid sh $ GV.fromList $ take (size sh) xs

replicate :: (Shape s, MGV.MVector v a, P.PrimMonad m)
  => s
  -> a
  -> m (Grid s (v (P.PrimState m)) a)
replicate sh x = Grid sh <$> MGV.replicate (size sh) x

length :: GV.Vector v a => Grid s v a -> Int
length = GV.length . buffer

map :: (GV.Vector v a, GV.Vector v b) => (a -> b) -> Grid s v a -> Grid s v b
map f (Grid sh buf) = Grid sh $ GV.map f buf

-- instance GV.Vector v => Functor (Grid s v) where
--   fmap = map

onShape :: Shape s => (s -> a) -> s -> Int -> a
onShape f sh = f . (`toShape` sh)

imap :: (Shape s, GV.Vector v a, GV.Vector v b)
  => (s -> a -> b)
  -> Grid s v a
  -> Grid s v b
imap f (Grid sh buf) = Grid sh $ GV.imap (f `onShape` sh) buf

filter :: GV.Vector v a => (a -> Bool) -> Grid s v a -> Grid s v a
filter p (Grid sh buf) = Grid sh $ GV.filter p buf

ifilter :: (Shape s, GV.Vector v a)
  => (s -> a -> Bool)
  -> Grid s v a
  -> Grid s v a
ifilter p (Grid sh buf) = Grid sh $ GV.ifilter (p `onShape` sh) buf

foldl' :: GV.Vector v b => (a -> b -> a) -> a -> Grid s v b -> a
foldl' f z (Grid _ buf) = GV.foldl' f z buf

sum :: (Num a, GV.Vector v a) => Grid s v a -> a
sum = foldl' (+) 0

accum :: (Shape s, GV.Vector v a)
  => (a -> b -> a)
  -> Grid s v a
  -> [(s, b)]
  -> Grid s v a
accum f (Grid sh buf) xs
  = Grid sh $ GV.accum f buf $ L.map (first (`fromShape` sh)) xs

modify :: (Shape s, MGV.MVector v a, P.PrimMonad m)
  => Grid s (v (P.PrimState m)) a
  -> (a -> a)
  -> s
  -> m ()
modify (Grid sh buf) f i = MGV.modify buf f (i `fromShape` sh)

freeze :: (GV.Vector v a, P.PrimMonad m)
  => Grid s (GV.Mutable v (P.PrimState m)) a
  -> m (Grid s v a)
freeze (Grid sh buf) = do
  buf' <- GV.freeze buf
  return $ Grid sh buf'
