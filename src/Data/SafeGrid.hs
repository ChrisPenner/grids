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
{-# LANGUAGE ViewPatterns #-}

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


newtype SafeGrid  (dims :: [Nat]) a =
  SafeGrid (V.Vector a)
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable)

type family KnownNats dims :: Constraint where
  KnownNats (x:xs) = (KnownNat x, KnownNats xs)
  KnownNats '[] = ()

type family SizeOfDims dims :: Nat where
  SizeOfDims (x:xs) = (x N.* SizeOfDims xs)
  SizeOfDims '[] = 1

data x :*: y = x :*: y
  deriving Show

infixr 9 :*:

type family Coords (dims :: [Nat]) where
  Coords '[n] = Finite n
  Coords '[] = ()
  Coords (n:xs) = Finite n :*: Coords xs

class Sizeable (dims :: [Nat]) where
  sizeof :: Proxy dims -> Int
  toCoord :: Proxy dims -> Finite (SizeOfDims dims) -> Coords dims
  fromCoord :: Proxy dims -> Coords dims -> Finite (SizeOfDims dims)

instance Sizeable '[] where
  sizeof _ = 1
  toCoord _ _ = ()
  fromCoord _ _ = 1

instance (KnownNat x) => Sizeable '[x] where
  sizeof _  = fromIntegral (L.natVal (Proxy @x))
  toCoord _ i = i
  fromCoord _ i = i

instance (KnownNat x, KnownNat (y N.* SizeOfDims xs), KnownNat (x N.* (y N.* SizeOfDims xs)), (Sizeable (y:xs))) => Sizeable (x : y : xs) where
  sizeof _  = fromIntegral (L.natVal (Proxy @(SizeOfDims (x:y:xs))))
  fromCoord _ ((getFinite -> a) :*: rest) = finite $ (a * fromIntegral (sizeof (Proxy @(y:xs)))) + getFinite (fromCoord (Proxy @(y:xs)) rest)
  toCoord _ (getFinite -> i) = finite (i `mod` currentDim) :*: toCoord (Proxy @(y:xs)) (finite (i `div` currentDim))
    where
      currentDim = fromIntegral $ L.natVal (Proxy @x)

instance (KnownNat (SizeOfDims dims), Sizeable dims) => Distributive (SafeGrid dims) where
  distribute = distributeRep

instance (Sizeable dims, KnownNat (SizeOfDims dims)) => Representable (SafeGrid dims) where
  type Rep (SafeGrid dims) = Coords dims
  index (SafeGrid v) ind = v V.! fromIntegral (getFinite (fromCoord (Proxy @dims) ind))
  tabulate f = SafeGrid $ V.generate (fromIntegral $ sizeof (Proxy @dims)) (f . toCoord (Proxy @dims) . finite . fromIntegral)

instance (ind ~ Coords dims, Sizeable dims, KnownNat (SizeOfDims dims)) => FunctorWithIndex ind (SafeGrid dims) where
  imap = imapRep

instance (ind ~ Coords dims, Sizeable dims, KnownNat (SizeOfDims dims)) => FoldableWithIndex ind (SafeGrid dims) where
  ifoldMap = ifoldMapRep

instance (ind ~ Coords dims, Sizeable dims, KnownNat (SizeOfDims dims)) => TraversableWithIndex ind (SafeGrid dims) where
  itraverse = itraverseRep
