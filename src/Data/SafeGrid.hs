{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.SafeGrid where

import           Data.Distributive
import           Data.Functor.Rep
import qualified Data.Vector                   as V
import           GHC.TypeLits                  as L
import           Data.Proxy
import           Data.Functor.Compose
import           Control.Lens
import           Data.Kind
import           GHC.TypeNats                  as N
import           Data.Finite
import           Control.Applicative

newtype Grid (dims :: [Nat]) a =
  Grid  (V.Vector a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Dimensions dims) => Applicative (Grid dims) where
  pure a = tabulate (const a)
  liftA2 f (Grid v) (Grid u) = Grid $ V.zipWith f v u

type family SizeOfDims dims :: Nat where
  SizeOfDims '[] = 0
  SizeOfDims (x:'[]) = x
  SizeOfDims (x:xs) = (x N.* SizeOfDims xs)

data x :# y = x :# y
  deriving (Show, Eq, Ord)

infixr 9 :#

type family Coords (dims :: [Nat]) where
  Coords '[n] = Finite n
  Coords (n:xs) = Finite n :# Coords xs

sizeof
  :: forall (dims :: [Nat]) . KnownNat (SizeOfDims dims) => Proxy dims -> Int
sizeof _ = fromIntegral (L.natVal (Proxy @(SizeOfDims dims)))

type NumericConstraints dims = (KnownNat (SizeOfDims dims))

type Dims = [Int]

class (NumericConstraints dims) => Dimensions (dims :: [Nat]) where
  toCoord :: Proxy dims -> Finite (SizeOfDims dims) -> Coords dims
  fromCoord :: Proxy dims -> Coords dims -> Finite (SizeOfDims dims)

instance (KnownNat x) => Dimensions '[x] where
  toCoord _ i = i
  fromCoord _ i = i

toCoord' :: Dims -> Int -> [Int]
toCoord' []       _ = []
toCoord' [_     ] n = [n]
toCoord' (_ : ds) n = (n `div` product ds) : toCoord' ds (n `mod` product ds)

fromCoord' :: Dims -> [Int] -> Int
fromCoord' _        []       = 1
fromCoord' _        [c     ] = c
fromCoord' (_ : ds) (c : cs) = c * product ds + fromCoord' ds cs

toFinite :: (KnownNat n) => Integral m => m -> Finite n
toFinite = finite . fromIntegral

fromFinite :: Num n => Finite m -> n
fromFinite = fromIntegral . getFinite

instance (KnownNat (x N.* SizeOfDims (y:xs)), KnownNat x, Dimensions (y:xs)) => Dimensions (x:y:xs) where
  toCoord _ n = firstCoord :# toCoord (Proxy @(y:xs)) remainder
    where
      firstCoord = toFinite (n `div` fromIntegral (sizeof (Proxy @(y:xs))))
      remainder = toFinite (fromFinite n `mod` sizeof (Proxy @(y:xs)))
  fromCoord _ (x :# ys) =
    toFinite $ firstPart + rest
      where
        firstPart = fromFinite x * sizeof (Proxy @(y:xs))
        rest = fromFinite (fromCoord (Proxy @(y:xs)) ys)

instance (Dimensions dims) => Distributive (Grid dims) where
  distribute = distributeRep

instance (Dimensions dims) => Representable (Grid dims) where
  type Rep (Grid dims) = Coords dims
  index (Grid v) ind = v V.! fromIntegral (fromCoord (Proxy @dims) ind)
  tabulate f = Grid $ V.generate (fromIntegral $ sizeof (Proxy @dims)) (f . toCoord (Proxy @dims) . fromIntegral)

instance (Dimensions dims, ind ~ Coords dims)
  => FunctorWithIndex ind (Grid dims) where
    imap = imapRep

instance (Dimensions dims, ind ~ Coords dims)
  => FoldableWithIndex ind (Grid dims) where
    ifoldMap = ifoldMapRep

instance (Dimensions dims, ind ~ Coords dims)
  => TraversableWithIndex ind (Grid dims) where
    itraverse = itraverseRep

testGrid1 :: Grid '[3, 3] Int
testGrid1 = tabulate (fromIntegral . fromCoord (Proxy @'[3, 3]))

testGrid2 :: Grid '[3, 3] Int
testGrid2 = tabulate ((* 10) . fromIntegral . fromCoord (Proxy @'[3, 3]))
