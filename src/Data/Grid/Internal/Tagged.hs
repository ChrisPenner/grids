{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Internal.Tagged where

import GHC.TypeNats
import Data.Proxy

newtype Tagged (n :: Nat) = Tagged {unTagged :: Int}
  deriving (Show, Num, Eq, Ord, Real, Enum, Integral)

inBounds :: forall n . KnownNat n => Tagged n -> Bool
inBounds (Tagged n) | n < 0 = False
inBounds (Tagged n) | n >= fromIntegral (natVal (Proxy @n)) = False
inBounds _                  = True
