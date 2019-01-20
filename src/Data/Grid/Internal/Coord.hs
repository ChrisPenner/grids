{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Grid.Internal.Coord where

import           GHC.Exts
import           GHC.TypeNats                      hiding ( Mod )
import           GHC.TypeLits hiding (natVal, Mod)
import           Data.Proxy
import           Data.Kind
import           Unsafe.Coerce
import           Data.Coerce
import           Data.List
import           Data.Singletons.Prelude

data Ind = Mod | Clamp

newtype Coord (dims :: [Nat]) (ind :: Ind) = Coord {unCoord :: [Int]}
  deriving (Eq)

instance IsList (Coord dims ind) where
  type Item (Coord dims ind) = Int
  fromList = coerce
  toList = coerce

instance Show (Coord dims ind)
  where
    show (Coord cs) = "(" ++ intercalate ", " (show <$> cs) ++ ")"

unconsC :: Coord (n : ns) ind -> (Int, Coord ns ind)
unconsC (Coord (n : ns)) = (n, Coord ns)

appendC :: Coord ns ind -> Coord ms ind -> Coord (ns ++ ms) ind
appendC (Coord ns) (Coord ms) = Coord (ns ++ ms)

pattern (:#) :: Int -> (Coord (ns) ind) -> Coord (n:ns) ind
pattern n :# ns <- (unconsC -> (n, ns)) where
  n :# (unCoord -> ns) = Coord (n:ns)

instance (Enum (Coord ns ind)) => Num (Coord ns ind) where
  (Coord xs) + (Coord ys) = Coord (zipWith (+) xs ys)
  a - b = a + (negate b)
  (Coord xs) * (Coord ys) = Coord (zipWith (*) xs ys)
  abs (Coord xs) = Coord (abs <$> xs)
  signum (Coord xs) = Coord (signum <$> xs)
  fromInteger = toEnum . fromIntegral
  negate (Coord xs) = Coord (negate <$> xs)

highestIndex :: forall n. KnownNat n => Int
highestIndex = fromIntegral $ natVal (Proxy @n) - 1

clamp :: (Int, Int) -> Int -> Int
clamp (start, end) = max start . min end

instance Bounded (Coord '[] ind) where
  minBound = Coord []
  maxBound = Coord []

instance (KnownNat n, Bounded (Coord ns ind)) => Bounded (Coord (n:ns) ind) where
  minBound = 0 :# minBound
  maxBound = highestIndex @n :# maxBound

instance  (KnownNat n) => Enum (Coord '[n] Mod) where
  toEnum i = Coord [i]
  fromEnum (Coord [i]) = i `mod` (highestIndex @n + 1)

instance  (KnownNat n) => Enum (Coord '[n] Clamp) where
  toEnum i = Coord [i]
  fromEnum (Coord [i]) = clamp (0, highestIndex @n) i

instance  (KnownNat x, KnownNat y, SingI rest, Bounded (Coord rest Clamp), Enum (Coord (y:rest) Clamp)) => Enum (Coord (x:y:rest) Clamp) where
  toEnum i | i < 0 = negate $ toEnum (abs i)
  toEnum i | i > fromEnum (maxBound @(Coord (x:y:rest) Clamp)) = error "Index out of bounds"
  toEnum i = (i `div` (inhabitants @(y:rest))) :# toEnum (i `mod` inhabitants @(y:rest))
  fromEnum (x :# ys) = (clamp (0, highestIndex @x) x * inhabitants @(y:rest)) + fromEnum ys

instance  (KnownNat x, KnownNat y, SingI rest, Bounded (Coord rest Mod), Enum (Coord (y:rest) Mod)) => Enum (Coord (x:y:rest) Mod) where
  toEnum i | i < 0 = negate $ toEnum (abs i)
  toEnum i | i > fromEnum (maxBound @(Coord (x:y:rest) Mod)) = error "Index out of bounds"
  toEnum i = (i `div` (inhabitants @(y:rest))) :# toEnum (i `mod` inhabitants @(y:rest))
  fromEnum (x :# ys) = ((x `mod` (highestIndex @x + 1))  * inhabitants @(y:rest)) + fromEnum ys

inhabitants :: forall (dims :: [Nat]) . SingI dims => Int
inhabitants = product . fmap fromIntegral $ demote @dims

coerceCoord :: Coord ns (i :: Ind) -> Coord ns (j :: Ind)
coerceCoord = unsafeCoerce

coerceCoordDims :: Coord ns i -> Coord ms i
coerceCoordDims = unsafeCoerce

coordInBounds :: forall ns i. (SingI ns) => Coord ns i -> Bool
coordInBounds (Coord cs) = all inRange $ zip cs maxIndexes
  where
    maxIndexes = fromIntegral <$> demote @ns
    inRange (val,upperBound) = val >= 0 && val < upperBound
