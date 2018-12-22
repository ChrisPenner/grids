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

type family Coords (dims :: [Nat]) where
  Coords '[n] = Finite n
  Coords '[] = ()
  Coords (n:xs) = Finite n :*: Coords xs

sizeof
  :: forall (dims :: [Nat]) . KnownNat (SizeOfDims dims) => Proxy dims -> Int
sizeof _ = fromIntegral (L.natVal (Proxy @(SizeOfDims dims)))

type NumericConstraints dims = (KnownNat (SizeOfDims dims))

type Dims = [Int]

toCoord' :: Dims -> Int -> [Int]
toCoord' dims n = undefined

fromCoord' :: Dims -> [Int] -> Int
fromCoord' dims coords = undefined

class (NumericConstraints dims) => Sizeable (dims :: [Nat]) where
  toCoord :: Proxy dims -> Finite (SizeOfDims dims) -> Coords dims
  fromCoord :: Proxy dims -> Coords dims -> Finite (SizeOfDims dims)

instance Sizeable '[] where
  toCoord _ _ = ()
  fromCoord _ _ = 0

instance (KnownNat x) => Sizeable '[x] where
  toCoord _ i = i
  fromCoord _ i = i

instance (NumericConstraints (x : y : xs), Sizeable (y:xs), NumericConstraints '[x])
  => Sizeable (x : y : xs) where
  fromCoord _ ((fromIntegral -> a) :*: rest) = fromIntegral $ (a * fromIntegral (sizeof (Proxy @(y:xs)))) + fromIntegral (fromCoord (Proxy @(y:xs)) rest)
  toCoord _ (fromIntegral -> i) = fromIntegral (i `mod` currentDim) :*: toCoord (Proxy @(y:xs)) (fromIntegral (i `div` currentDim))
    where
      currentDim = fromIntegral $ L.natVal (Proxy @x)

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

testGrid :: SGrid '[2, 3] (Finite 2 :*: Finite 3)
testGrid = tabulate id
