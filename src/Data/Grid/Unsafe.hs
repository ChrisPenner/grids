{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Grid.Unsafe where

import qualified Data.Vector                   as V
import Control.Applicative
import Data.Functor.Compose

type Dims = [Int]
type Coord = [Int]

data Grid a =
  Grid Dims (V.Vector a)
  deriving (Eq, Functor)

instance (Show a) => Show (Grid a) where
  show (Grid d g) = "(Grid " ++ show d ++ show g ++ ")"

instance (Semigroup a) => Semigroup (Grid a) where
  (<>) = lift2 (<>)

lift2 :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
lift2 f (Grid d a) (Grid d' b)
  | d /= d'   = error "cannot combine differently sized grids"
  | otherwise = Grid d $ V.zipWith f a b

index :: Grid a -> Coord -> a
index (Grid d v) crd = v V.! fromCoord d crd

tabulate :: [Int] -> (Coord -> a) -> Grid a
tabulate d f = Grid d $ V.generate (product d) (f . toCoord d)

gridSize :: Grid a -> Int
gridSize (Grid ds _) = product ds

gridSize' :: Dims -> Int
gridSize' = product

fromCoord :: Dims -> Coord -> Int
fromCoord []      []      = 0
fromCoord []      (_ : _) = error "too many coordinates provided"
fromCoord (_ : _) []      = error "not enough coordinates provided"
fromCoord (d : _) (x : _) | x >= d =
  error
    $  "Grid coord out of range, "
    ++ show x
    ++ " must be less than "
    ++ show d
fromCoord (_ : ds) (x : ys) = firstPart + rest
 where
  firstPart = x * gridSize' ds
  rest      = fromCoord ds ys

toCoord :: Dims -> Int -> Coord
toCoord []       0 = []
toCoord (d : ds) n = if firstCoord >= d
  then error "coordinate is out of range"
  else firstCoord : toCoord ds remainder
 where
  firstCoord = n `div` gridSize' ds
  remainder  = n `mod` gridSize' ds

class Griddable l where
  toGrid :: l -> Grid (Member l)
  dims :: l -> Dims
  toVect :: l -> V.Vector (Member l)

type family Member t where
  Member [[a]] = Member [a]
  Member [a] = a

instance {-# OVERLAPPING #-} (Griddable [l]) => Griddable [[l]] where
  toGrid xs@(x:_) = Grid (length xs : dims x) (V.concat $ fmap toVect xs)
  dims xs@(x:_) = length xs : dims x
  toVect xs = V.concat $ fmap toVect xs

instance (Member [a] ~ a) => Griddable [a] where
  toGrid xs = Grid [length xs] (V.fromList xs)
  dims xs = [length xs]
  toVect = V.fromList
