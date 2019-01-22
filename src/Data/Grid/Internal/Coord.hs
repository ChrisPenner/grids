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

-- | The index type for 'Grid's.
newtype Coord (dims :: [Nat]) = Coord {unCoord :: [Int]}
  deriving (Eq)

-- | Safely construct a 'Coord' for a given grid size, checking that all
-- indexes are in range
--
-- > λ> coord @[3, 3] [1, 2]
-- > Just [1, 2]
-- >
-- > λ> coord @[3, 3] [3, 3]
-- > Nothing
-- >
-- > λ> coord @[3, 3] [1, 2, 3]
-- > Nothing
coord :: forall dims. SingI dims => [Int] -> Maybe (Coord dims)
coord ds = if inRange && correctLength then Just (Coord ds)
                      else Nothing
  where
    inRange = all (>=0) ds && all id (zipWith (<) ds (fromIntegral <$> demote @dims))
    correctLength = length ds == length (demote @dims)

instance IsList (Coord dims) where
  type Item (Coord dims) = Int
  fromList = coerce
  toList = coerce

instance Show (Coord dims)
  where
    show (Coord cs) = "[" ++ intercalate ", " (show <$> cs) ++ "]"

-- | Get the first index from a 'Coord'
unconsC :: Coord (n : ns) -> (Int, Coord ns)
unconsC (Coord (n : ns)) = (n, Coord ns)

-- | Append two 'Coord's
appendC :: Coord ns -> Coord ms -> Coord (ns ++ ms) 
appendC (Coord ns) (Coord ms) = Coord (ns ++ ms)

pattern (:#) :: Int -> Coord ns -> Coord (n:ns) 
pattern n :# ns <- (unconsC -> (n, ns)) where
  n :# (unCoord -> ns) = Coord (n:ns)

instance (Enum (Coord ns)) => Num (Coord ns ) where
  (Coord xs) + (Coord ys) = Coord (zipWith (+) xs ys)
  a - b = a + (negate b)
  (Coord xs) * (Coord ys) = Coord (zipWith (*) xs ys)
  abs (Coord xs) = Coord (abs <$> xs)
  signum (Coord xs) = Coord (signum <$> xs)
  fromInteger = toEnum . fromIntegral
  negate (Coord xs) = Coord (negate <$> xs)

highestIndex :: forall n. KnownNat n => Int
highestIndex = fromIntegral $ natVal (Proxy @n) - 1

clamp :: Int -> Int -> Int -> Int
clamp start end = max start . min end

clampCoord :: forall dims. SingI dims => Coord dims -> Coord dims
clampCoord (Coord ns) = Coord (zipWith (clamp 0 . fromIntegral) (demote @dims) ns)

wrapCoord :: forall dims. SingI dims => Coord dims -> Coord dims
wrapCoord (Coord ns) = Coord (zipWith mod  ns (fromIntegral <$> demote @dims)) 

instance Bounded (Coord '[] ) where
  minBound = Coord []
  maxBound = Coord []

instance (KnownNat n, Bounded (Coord ns )) => Bounded (Coord (n:ns) ) where
  minBound = 0 :# minBound
  maxBound = highestIndex @n :# maxBound

instance  (KnownNat n) => Enum (Coord '[n]) where
  toEnum i = Coord [i]
  fromEnum (Coord [i]) = clamp 0 (highestIndex @n) i

instance  (KnownNat x, KnownNat y, SingI rest, Bounded (Coord rest ), Enum (Coord (y:rest) )) => Enum (Coord (x:y:rest) ) where
  toEnum i | i < 0 = negate $ toEnum (abs i)
  toEnum i | i > fromEnum (maxBound @(Coord (x:y:rest) )) = error "Index out of bounds"
  toEnum i = (i `div` (gridSize @(y:rest))) :# toEnum (i `mod` gridSize @(y:rest))
  fromEnum (x :# ys) = (clamp 0 (highestIndex @x) x * gridSize @(y:rest)) + fromEnum ys

gridSize :: forall (dims :: [Nat]) . SingI dims => Int
gridSize = product . fmap fromIntegral $ demote @dims

coerceCoordDims :: Coord ns -> Coord ms
coerceCoordDims = unsafeCoerce

coordInBounds :: forall ns. (SingI ns) => Coord ns -> Bool
coordInBounds (Coord cs) = all inRange $ zip cs maxIndexes
  where
    maxIndexes = fromIntegral <$> demote @ns
    inRange (val,upperBound) = val >= 0 && val < upperBound
