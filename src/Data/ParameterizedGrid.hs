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

module Data.ParameterizedGrid where

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

type Grid (dims :: [Nat]) a = PGrid Unsafe dims a
type Grid' (dims :: [Nat]) a = PGrid Safe dims a

newtype PGrid (s :: Safeness) (dims :: [Nat]) a =
  PGrid (V.Vector a)
  deriving (Show, Eq, Functor, Applicative, Foldable, Traversable)

type family KnownNats dims :: Constraint where
  KnownNats (x:xs) = (KnownNat x, KnownNats xs)
  KnownNats '[] = ()

type family SizeOfDims dims :: Nat where
  SizeOfDims '[] = 0
  SizeOfDims (x:'[]) = x
  SizeOfDims (x:xs) = (x N.* SizeOfDims xs)

data x :*: y = x :*: y
  deriving Show

infixr 9 :*:

data Safeness = Unsafe | Safe

type family WrapSafe (s :: Safeness) (n :: Nat) where
  WrapSafe Safe n = Finite n
  WrapSafe Unsafe _ = Int

type family Coords (s :: Safeness) (dims :: [Nat]) where
  Coords s '[n] = WrapSafe s n
  Coords _ '[] = ()
  Coords s (n:xs) = WrapSafe s n :*: Coords s xs

class Sizeable (s :: Safeness) (dims :: [Nat]) where
  sizeof :: Proxy '(s, dims) -> Int
  toCoord :: Proxy '(s, dims) -> WrapSafe s (SizeOfDims dims) -> Coords s dims
  fromCoord :: Proxy '(s, dims) -> Coords s dims -> WrapSafe s (SizeOfDims dims)

class SafeToInt s n where
  safeToInt ::  Proxy '(s, n) -> WrapSafe s n -> Int
  safeFromInt ::  Proxy '(s, n) -> Int -> WrapSafe s n

instance SafeToInt Unsafe n where
  safeToInt _ i = i
  safeFromInt _ i = i

instance (KnownNat n) => SafeToInt Safe n where
  safeToInt _ i = fromIntegral $ getFinite i
  safeFromInt _ i = finite $ fromIntegral i

instance (Num (WrapSafe s 0)) => Sizeable s '[] where
  sizeof _ = 0
  toCoord _ _ = ()
  fromCoord _ _ = 0

instance (KnownNat x) => Sizeable s '[x] where
  sizeof _  = fromIntegral (L.natVal (Proxy @x))
  toCoord _ i = i
  fromCoord _ i = i

instance (SafeToInt s x, SafeToInt s (x N.* SizeOfDims (y : xs)), SafeToInt s (SizeOfDims (y:xs)), KnownNat x, (KnownNat (SizeOfDims (y : xs))), Sizeable s (y : xs), (KnownNat (x N.* SizeOfDims (y : xs))))
  => Sizeable s (x : y : xs) where
  sizeof _  = fromIntegral (L.natVal (Proxy @(SizeOfDims (x:y:xs))))
  fromCoord _ ((safeToInt (Proxy @'(s, x)) -> a) :*: rest) = safeFromInt (Proxy @'(s, SizeOfDims (x : y : xs))) $ (a * fromIntegral (sizeof (Proxy @'(s, y:xs)))) + safeToInt (Proxy @'(s, SizeOfDims (y : xs))) (fromCoord (Proxy @'(s, y:xs)) rest)
  toCoord _ (safeToInt (Proxy @'(s, SizeOfDims (x : y : xs))) -> i) = safeFromInt (Proxy @'(s, x)) (i `mod` currentDim) :*: toCoord (Proxy @'(s, y:xs)) (safeFromInt (Proxy @'(s, SizeOfDims (y : xs))) $  (i `div` currentDim))
    where
      currentDim = fromIntegral $ L.natVal (Proxy @x)

instance (SafeToInt s (SizeOfDims dims), KnownNat (SizeOfDims dims), Sizeable s dims) => Distributive (PGrid s dims) where
  distribute = distributeRep

instance (SafeToInt s (SizeOfDims dims), Sizeable s dims, KnownNat (SizeOfDims dims)) => Representable (PGrid s dims) where
  type Rep (PGrid s dims) = Coords s dims
  index (PGrid v) ind = v V.! fromIntegral (safeToInt (Proxy @'(s, SizeOfDims dims)) (fromCoord (Proxy @'(s, dims)) ind))
  tabulate f = PGrid $ V.generate (fromIntegral $ sizeof (Proxy @'(s, dims))) (f . toCoord (Proxy @'(s, dims)) . safeFromInt (Proxy @'(s, SizeOfDims dims)))

instance (SafeToInt s (SizeOfDims dims), ind ~ Coords s dims, Sizeable s dims, KnownNat (SizeOfDims dims)) => FunctorWithIndex ind (PGrid s dims) where
  imap = imapRep

instance (SafeToInt s (SizeOfDims dims),ind ~ Coords s dims, Sizeable s dims, KnownNat (SizeOfDims dims)) => FoldableWithIndex ind (PGrid s dims) where
  ifoldMap = ifoldMapRep

instance (SafeToInt s (SizeOfDims dims), ind ~ Coords s dims, Sizeable s dims, KnownNat (SizeOfDims dims)) => TraversableWithIndex ind (PGrid s dims) where
  itraverse = itraverseRep
