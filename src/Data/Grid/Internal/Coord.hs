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

import           Data.Grid.Internal.Index

import           GHC.TypeNats                      hiding ( Mod )
import           GHC.TypeLits hiding (natVal, Mod)
import           Data.Proxy
import           Data.Kind
import           Unsafe.Coerce
import           Data.Coerce
import           Data.List
import           Data.Singletons.Prelude

data Coord (dims :: [Nat]) (ind :: Ind) = Coord {unCoord :: [Int]}

instance Show (Coord dims ind)
  where
    show (Coord cs) = "(" ++ intercalate ", " (show <$> cs) ++ ")"

unconsC :: Coord (n : ns) ind -> (Int, Coord ns ind)
unconsC (Coord (n : ns)) = (n, Coord ns)

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

instance {-# OVERLAPPING #-} (KnownNat n) => Enum (Coord '[n] Mod) where
  toEnum i = Coord [i]
  fromEnum (Coord [i]) = i `mod` highestIndex @n

instance {-# OVERLAPPING #-} (KnownNat n) => Enum (Coord '[n] Clamp) where
  toEnum i = Coord [i]
  fromEnum (Coord [i]) = clamp (0, highestIndex @n) i

instance (Enum (Coord ns ind), Bounded (Coord ns ind)) => Enum (Coord (n:ns) ind) where
  toEnum i | i < 0 = negate $ toEnum (abs i)
  toEnum i = toEnum (i `div` membersOfY) :# toEnum (i `mod` membersOfY)
    where
      membersOfY = fromEnum (inhabitants @(Coord ns ind))
  fromEnum (x :# y) = (fromEnum x * fromEnum (inhabitants @(Coord ns ind))) + fromEnum y

inhabitants :: forall x . (Bounded x, Enum x) => Int
inhabitants = fromEnum (maxBound @x) + 1

coerceCoord :: Coord ns (i :: Ind) -> Coord ns (j :: Ind)
coerceCoord = unsafeCoerce

coerceCoordDims :: Coord ns i -> Coord ms i
coerceCoordDims c = unsafeCoerce c

coordInBounds :: forall ns i. (SingI ns) => Coord ns i -> Bool
coordInBounds (Coord cs) = all inRange $ zip cs maxIndexes
  where
    maxIndexes = fromIntegral <$> demote @ns
    inRange (val,upperBound) = val >= 0 && val < upperBound
