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


module Data.Grid.Internal.Coord where

import           Data.Grid.Internal.Mod
import           Data.Grid.Internal.Tagged
import           Data.Grid.Internal.Clamp
import           Data.Grid.Internal.Dims
import           Data.Grid.Internal.Index

import           GHC.TypeNats                      hiding ( Mod )
import           Data.Proxy
import           Data.Finite
import           Data.Kind


data Coord (dims :: [Nat]) (ind :: Ind) where
  (:#) :: KnownNat n => Index n ind -> Coord ns ind -> Coord (n:ns) ind
  Coord :: KnownNat n => Index n ind -> Coord '[n] ind

infixr 9 :#

instance {-# OVERLAPPING #-} (KnownNat n) => Num (Coord '[n] ind) where
  (Coord x) + (Coord y) = Coord (x + y)
  a - b = a + (-b)
  (Coord x) * (Coord y) = Coord (x * y)
  abs (Coord x) = Coord (abs x)
  signum (Coord x) = Coord (signum x)
  fromInteger i = Coord (fromIntegral i)
  negate (Coord x) = Coord (negate x)


instance (Enum (Coord ns ind), Num (Coord ns ind), KnownNat n, Enum (Index n ind), Bounded (Coord ns ind)) => Num (Coord (n:ns) ind) where
  (x :# y) + (x' :# y') = (x + x') :# (y + y')
  a - b = a + (-b)
  (x :# y) * (x' :# y') = (x * x') :# (y * y')
  abs (x :# y) = abs x :# abs y
  signum (x :# y) = signum x :# signum y
  fromInteger = toEnum . fromIntegral
  negate (x :# y) = negate x :# negate y

instance (KnownNat n, Bounded (Coord ns ind)) => Bounded (Coord (n:ns) ind) where
  minBound = minBound :# minBound
  maxBound = maxBound :# maxBound

instance (KnownNat n, Num (Coord (n:ns) ind), Enum (Index n ind), Enum (Coord ns ind), Bounded (Coord ns ind)) => Enum (Coord (n:ns) ind) where
  toEnum i | i < 0 = negate $ toEnum (abs i)
  toEnum i = toEnum (i `div` membersOfY) :# toEnum (i `mod` membersOfY)
    where
      membersOfY = fromEnum (inhabitants @(Coord ns ind))
  fromEnum (x :# y) = (fromEnum x * fromEnum (inhabitants @(Coord ns ind))) + fromEnum y

-- instance {-# OVERLAPPABLE #-} (Integral i) => AsIndex i n where
--   toIndex _ = fromIntegral
--   fromIndex _ = fromIntegral

-- class AsCoord c (dims :: [Nat]) where
--   toCoord :: Proxy dims -> Int -> c
--   fromCoord :: Proxy dims -> c -> Int

-- instance (KnownNat x) => AsCoord [Int] '[x] where
--   toCoord _ n =
--     if n >= fromIntegral (natVal (Proxy @x))
--                        then error $ "coordinate out of bounds: " ++ show n
--                        else [n]
--   fromCoord _ [n] = if n >= fromIntegral (natVal (Proxy @x))
--                        then error $ "coordinate out of bounds: " ++ show n
--                        else n
--   fromCoord _ _ = error "coordinate mismatch"

-- instance (KnownNat x, KnownNat y, Sizeable xs, AsCoord [Int] (y:xs)) => AsCoord [Int] (x:y:xs) where
--   toCoord _ n = if firstCoord >= fromIntegral (natVal (Proxy @x))
--                    then error ("coordinate out of bounds: " ++ show n)
--                    else firstCoord : toCoord (Proxy @(y:xs)) remainder
--     where
--       firstCoord = n `div` fromIntegral (gridSize (Proxy @(y:xs)))
--       remainder = n `mod` gridSize (Proxy @(y:xs))
--   fromCoord _ (x : ys) = if x >= fromIntegral (natVal (Proxy @x))
--                    then error ("coordinate out of bounds: " ++ show x)
--                    else firstPart + rest
--       where
--         firstPart = x * gridSize (Proxy @(y:xs))
--         rest = fromCoord (Proxy @(y:xs)) ys
--   fromCoord _ _ = error "coordinate mismatch"

-- instance AsCoord () '[] where
--   toCoord _ _ = ()
--   fromCoord _ () = 0

-- instance {-# OVERLAPPABLE #-} (AsIndex c n) => AsCoord c '[n] where
--   toCoord _ = toIndex (Proxy @n)
--   fromCoord _ = fromIndex (Proxy @n)

-- -- This instance is unnecessarily complicated to prevent overlapping instances
-- instance (KnownNat x, KnownNat y, Sizeable xs, AsIndex xInd x, AsCoord xsCoord (y:xs)) => AsCoord (xInd :# xsCoord) (x:y:xs) where
--   toCoord _ n = firstCoord :# toCoord (Proxy @(y:xs)) remainder
--     where
--       firstCoord = toIndex (Proxy @x) (n `div` fromIntegral (gridSize (Proxy @(y:xs))))
--       remainder = n `mod` gridSize (Proxy @(y:xs))
--   fromCoord _ (x :# ys) =
--      toIndex (Proxy @x) (firstPart + rest)
--       where
--         firstPart = fromIndex (Proxy @x) x * gridSize (Proxy @(y:xs))
--         rest = fromCoord (Proxy @(y:xs)) ys

-- | The coordinate type for a given dimensionality
--
-- > Coord [2, 3] == Finite 2 :# Finite 3
-- > Coord [4, 3, 2] == Finite 4 :# Finite 3 :# Finite 2
-- type family Coord (ind :: Ind) (dims :: [Nat]) where
--   Coord L '[_] = Int
--   Coord L (_:_) = [Int]
--   Coord ind '[n] = IndexOf ind n
--   Coord ind (n:xs) = IndexOf ind n :# Coord ind xs

-- type family IndexOf (ind :: Ind) (n :: Nat)
-- type instance IndexOf O n = Finite n
-- type instance IndexOf M n = Mod n
-- type instance IndexOf C n = Clamp n
-- type instance IndexOf L n = Int
-- type instance IndexOf T n = Tagged n

inhabitants :: forall x . (Bounded x, Enum x) => Int
inhabitants = fromEnum (maxBound @x) + 1

class SoftBounded c where
  inBounds :: c -> Bool

instance {-# OVERLAPPABLE #-} (Bounded c, Ord c) => SoftBounded c where
  inBounds c = c >= minBound && c <= maxBound

instance {-# OVERLAPPING #-} (SoftBounded (Index n ind), SoftBounded (Coord ns ind)) => SoftBounded (Coord (n:ns) ind) where
  inBounds (x :# y) = inBounds x && inBounds y
