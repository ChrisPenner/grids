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

newClamp :: forall m . (KnownNat m) => Int -> Clamp m
newClamp n = Clamp (max 0 $ min n (fromIntegral (natVal (Proxy @m))))

unClamp :: Integral i => Clamp n -> i
unClamp (Clamp n) = fromIntegral n

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
