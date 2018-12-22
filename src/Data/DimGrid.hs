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

module Data.DimGrid where

import Data.Distributive
import Data.Functor.Rep
import qualified Data.Vector as V
import GHC.TypeLits as L
import Data.Proxy
import Data.Functor.Compose
import Control.Lens
import Data.Kind
import GHC.TypeNats as N


newtype DimGrid  (dims :: [Nat]) a =
  DimGrid (V.Vector a)
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

type family Coords (dims :: [Nat]) = result where
  Coords '[_] = Int
  Coords '[] = ()
  Coords (_:xs) = Int :*: Coords xs



instance (KnownNat (SizeOfDims dims), Sizeable dims) => Distributive (DimGrid dims) where
  distribute = distributeRep

class Sizeable (dims :: [Nat]) where
  sizeof :: Proxy dims -> Int
  toCoord :: Proxy dims -> Int -> Coords dims
  fromCoord :: Proxy dims -> Coords dims -> Int

instance Sizeable '[] where
  sizeof _ = 1
  toCoord _ _ = ()
  fromCoord _ _ = 1

instance (KnownNat x) => Sizeable '[x] where
  sizeof _  = fromIntegral (L.natVal (Proxy @x))
  toCoord _ i = i
  fromCoord _ i = i

instance (KnownNat x, KnownNat (x N.* (y N.* SizeOfDims xs)), (Sizeable (y:xs))) => Sizeable (x : y : xs) where
  sizeof _  = fromIntegral (L.natVal (Proxy @(SizeOfDims (x:y:xs))))
  fromCoord _ (a :*: rest) = (a * sizeof (Proxy @(y:xs))) + fromCoord (Proxy @(y:xs)) rest
  toCoord _ i = (i `mod` currentDim) :*: toCoord (Proxy @(y:xs)) (i `div` currentDim)
    where
      currentDim = fromIntegral $ L.natVal (Proxy @x)
      currentSize = sizeof (Proxy @(y:xs))

instance (Sizeable dims, KnownNat (SizeOfDims dims)) => Representable (DimGrid dims) where
  type Rep (DimGrid dims) = Coords dims
  index (DimGrid v) ind = v V.! fromCoord (Proxy @dims) ind
  tabulate f = DimGrid $ V.generate (sizeof (Proxy @dims)) (f . toCoord (Proxy @dims))


-- instance (KnownNat w, KnownNat h) => FunctorWithIndex (Int, Int) (Grid w h) where
--   imap = imapRep

-- instance (KnownNat w, KnownNat h) => FoldableWithIndex (Int, Int) (Grid w h) where
--   ifoldMap = ifoldMapRep

-- instance (KnownNat w, KnownNat h) => TraversableWithIndex (Int, Int) (Grid w h) where
--   itraverse = itraverseRep


testGrid :: DimGrid '[3] Int
testGrid = tabulate id

testGrid2 :: DimGrid '[2, 2] (Int :*: Int)
testGrid2 = tabulate id


testGrid3 :: DimGrid '[3, 3, 3] (Int :*: Int :*: Int)
testGrid3 = tabulate id
