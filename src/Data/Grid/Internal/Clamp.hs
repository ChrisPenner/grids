{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Data.Grid.Internal.Clamp
  ( Clamp
  , newClamp
  , unClamp
  )
where

import           GHC.TypeNats
import           Data.Proxy

newtype Clamp (n :: Nat) = Clamp Int
  deriving (Show, Eq, Ord)

newClamp :: (Integral i) => i -> Clamp m
newClamp = Clamp . fromIntegral

unClamp :: forall n i . (KnownNat n, Integral i) => Clamp n -> i
unClamp (Clamp n) = fromIntegral (max 0 . min upperBound $ n)
 where
  lowerBound = 0
  upperBound = max 0 (fromIntegral (natVal (Proxy @n)) - 1)

instance (KnownNat n) => Num (Clamp n) where
  Clamp a + Clamp b = newClamp (a + b)
  Clamp a - Clamp b = Clamp a + Clamp (-b)
  Clamp a * Clamp b = newClamp (a * b)
  abs (Clamp a) = Clamp (abs a)
  signum = const (newClamp 1)
  fromInteger = newClamp . fromIntegral
  negate (Clamp n) = newClamp (-n)

instance KnownNat n => Semigroup (Clamp n) where
  (<>) = (+)

instance KnownNat n => Monoid (Clamp n) where
  mempty = Clamp 0
