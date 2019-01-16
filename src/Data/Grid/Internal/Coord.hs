{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Grid.Internal.Coord where

import           Data.Grid.Internal.Mod
import           Data.Grid.Internal.Dims

import           GHC.TypeNats                      hiding ( Mod )
import           Data.Proxy
import           Data.Finite

-- | Used for constructing arbitrary depth coordinate lists 
-- e.g. @('Finite' 2 ':#' 'Finite' 3)@
data x :# y = x :# y
  deriving (Show, Eq, Ord)

infixr 9 :#

class AsIndex c (n :: Nat) where
  toIndex :: Proxy n -> Int -> c
  fromIndex :: Proxy n -> c -> Int

instance (KnownNat n) => AsIndex (Finite n) n where
  toIndex _ = fromIntegral
  fromIndex _ = fromIntegral

instance (KnownNat n) => AsIndex (Mod n) n where
  toIndex _ = newMod
  fromIndex _ = unMod

instance {-# OVERLAPPABLE #-} (Integral i) => AsIndex i n where
  toIndex _ = fromIntegral
  fromIndex _ = fromIntegral

class AsCoord c (dims :: [Nat]) where
  toCoord :: Proxy dims -> Int -> c
  fromCoord :: Proxy dims -> c -> Int

instance AsCoord () '[] where
  toCoord _ _ = ()
  fromCoord _ () = 0

instance {-# OVERLAPPABLE #-} (AsIndex c n) => AsCoord c '[n] where
  toCoord _ = toIndex (Proxy @n)
  fromCoord _ = fromIndex (Proxy @n)

-- This instance is unnecessarily complicated to prevent overlapping instances
instance (KnownNat y, Sizeable xs, AsIndex xInd x, AsCoord xsCoord (y:xs)) => AsCoord (xInd :# xsCoord) (x:y:xs) where
  toCoord _ n = firstCoord :# toCoord (Proxy @(y:xs)) remainder
    where
      firstCoord = toIndex (Proxy @x) (n `div` fromIntegral (gridSize (Proxy @(y:xs))))
      remainder = n `mod` gridSize (Proxy @(y:xs))
  fromCoord _ (x :# ys) =
     toIndex (Proxy @x) (firstPart + rest)
      where
        firstPart = fromIndex (Proxy @x) x * gridSize (Proxy @(y:xs))
        rest = fromCoord (Proxy @(y:xs)) ys
