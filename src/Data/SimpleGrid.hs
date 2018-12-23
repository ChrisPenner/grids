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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.SimpleGrid where

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
import Data.Singletons.Prelude

data SGrid (dims :: [Nat]) a =
  SGrid [Int] (V.Vector a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

type family SizeOfDims dims :: Nat where
  SizeOfDims '[] = 0
  SizeOfDims (x:'[]) = x
  SizeOfDims (x:xs) = (x N.* SizeOfDims xs)

data x :*: y = x :*: y
  deriving Show

infixr 9 :*:

class FoldCoord p where
  foldCoord :: p -> [Int]

instance (FoldCoord y) => FoldCoord (Int :*: y) where
  foldCoord (x :*: y) = x : foldCoord y

instance FoldCoord Int where
  foldCoord x = [x]

type family Coords (dims :: [Nat]) where
  Coords '[n] = Finite n
  Coords (n:xs) = Finite n :*: Coords xs

sizeof
  :: forall (dims :: [Nat]) . KnownNat (SizeOfDims dims) => Proxy dims -> Int
sizeof _ = fromIntegral (L.natVal (Proxy @(SizeOfDims dims)))

type NumericConstraints dims = (KnownNat (SizeOfDims dims))

type Dims = [Int]

class (NumericConstraints dims) => Sizeable (dims :: [Nat]) where
  toCoord :: Proxy dims -> Finite (SizeOfDims dims) -> Coords dims
  fromCoord :: Proxy dims -> Coords dims -> Finite (SizeOfDims dims)

instance (KnownNat x) => Sizeable '[x] where
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

instance (KnownNat (x N.* SizeOfDims (y:xs)), KnownNat x, Sizeable (y:xs)) => Sizeable (x:y:xs) where
  toCoord _ n = firstCoord :*: toCoord (Proxy @(y:xs)) remainder
    where
      firstCoord = toFinite (n `div` fromIntegral (sizeof (Proxy @(y:xs))))
      remainder = toFinite (fromFinite n `mod` sizeof (Proxy @(y:xs)))
  fromCoord _ (x :*: ys) =
    toFinite $ firstPart + rest
      where
        firstPart = fromFinite x * sizeof (Proxy @(y:xs))
        rest = fromFinite (fromCoord (Proxy @(y:xs)) ys)

instance (Sizeable dims, SingI dims) => Distributive (SGrid dims) where
  distribute = distributeRep

instance (Sizeable dims, SingI dims) => Representable (SGrid dims) where
  type Rep (SGrid dims) = Coords dims
  index (SGrid _ v) ind = v V.! fromIntegral (fromCoord (Proxy @dims) ind)
  tabulate f = SGrid (fromIntegral <$> demote @dims) $ V.generate (fromIntegral $ sizeof (Proxy @dims)) (f . toCoord (Proxy @dims) . fromIntegral)

instance (SingI dims, Sizeable dims, ind ~ Coords dims)
  => FunctorWithIndex ind (SGrid dims) where
    imap = imapRep

instance (SingI dims, Sizeable dims, ind ~ Coords dims)
  => FoldableWithIndex ind (SGrid dims) where
    ifoldMap = ifoldMapRep

instance (SingI dims, Sizeable dims, ind ~ Coords dims)
  => TraversableWithIndex ind (SGrid dims) where
    itraverse = itraverseRep
