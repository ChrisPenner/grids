{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Internal.Mod
  ( Mod
  , newMod
  , unMod
  )
where

import GHC.TypeNats hiding (Mod)
import Data.Proxy

newtype Mod (n :: Nat) = Mod Int
  deriving (Show, Eq)

newMod :: forall m . (KnownNat m) => Int -> Mod m
newMod n = Mod (n `mod` modulus)
  where modulus = fromIntegral $ natVal (Proxy :: Proxy m)

unMod :: Integral i => Mod n -> i
unMod (Mod n) = fromIntegral n

instance (KnownNat n) => Num (Mod n) where
  Mod a + Mod b = newMod (a + b)
  Mod a - Mod b = Mod a + Mod (-b)
  Mod a * Mod b = newMod (a * b)
  abs (Mod a) = Mod (abs a)
  signum = const (newMod 1)
  fromInteger = newMod . fromIntegral
  negate (Mod n) = newMod (-n)

instance KnownNat n => Semigroup (Mod n) where
  (<>) = (+)

instance KnownNat n => Monoid (Mod n) where
  mempty = Mod 0