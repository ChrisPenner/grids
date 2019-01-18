{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Grid.Internal.Index where

import GHC.TypeNats
import GHC.TypeLits hiding (natVal)
import Data.Proxy
import Data.Kind
import Data.Coerce

data Ind = O | M | C | L | T
data Index (n :: Nat) (ind :: Ind) where
  Index :: KnownNat n => Int -> Index n ind
  -- deriving ( Num, Ord, Eq)

instance (KnownSymbol (ShowIndex ind)) => Show (Index n ind) where
  show (Index n) =  symbolVal (Proxy @(ShowIndex ind)) ++ " " ++ show n

deriving instance Ord (Index n ind)
deriving instance Eq (Index n ind)

instance (KnownNat n) => Num (Index n ind) where
  Index a + Index b = Index (a + b)
  Index a - Index b = Index a + Index (-b)
  Index a * Index b = Index (a * b)
  abs (Index a) = Index (abs a)
  signum (Index i) = Index $ signum i
  fromInteger = Index . fromIntegral
  negate (Index n) = Index (-n)

instance (KnownNat n) => Bounded (Index n ind) where
  minBound = 0
  maxBound = fromIntegral (natVal (Proxy @n)) - 1

instance (KnownNat n) => Enum (Index n C) where
  toEnum = Index
  fromEnum (Index n) = fromIntegral (max minBound . min maxBound $ n)

instance (KnownNat n) => Enum (Index n M) where
  toEnum = Index
  fromEnum (Index n) = fromIntegral (n `mod` modulus)
    where modulus = fromIntegral $ natVal (Proxy @n)

type family ShowIndex (i::Ind) :: Symbol where
  ShowIndex C = "C"

coerceIndex :: Index n (i :: Ind) -> Index n (j :: Ind)
coerceIndex = coerce

coerceDims :: (KnownNat m) => Index n i -> Index m i
coerceDims (Index i) = Index i

indexInBounds :: (KnownNat n) => Index n i -> Bool
indexInBounds i = i >= minBound && i <= maxBound
