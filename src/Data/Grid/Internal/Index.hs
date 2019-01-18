{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Grid.Internal.Index where

import GHC.TypeNats hiding (Mod)
import GHC.TypeLits hiding (natVal, Mod)
import Data.Proxy
import Data.Kind
import Data.Coerce
import Unsafe.Coerce

data Ind = Mod | Clamp
data Index (n :: Nat) (ind :: Ind) where
  Index :: (KnownNat n, Enum (Index n ind), Num (Index n ind)) => Int -> Index n ind

instance (KnownSymbol (ShowIndex ind)) => Show (Index n ind) where
  show (Index n) =  symbolVal (Proxy @(ShowIndex ind)) ++ " " ++ show n

deriving instance Ord (Index n ind)
deriving instance Eq (Index n ind)

instance (KnownNat n, Enum (Index n ind)) => Num (Index n ind) where
  Index a + Index b = Index (a + b)
  Index a - Index b = Index a + Index (-b)
  Index a * Index b = Index (a * b)
  abs (Index a) = Index (abs a)
  signum (Index i) = Index $ signum i
  fromInteger = Index . fromIntegral
  negate (Index n) = Index (-n)

instance (KnownNat n, Enum (Index n ind)) => Bounded (Index n ind) where
  minBound = 0
  maxBound = fromIntegral (natVal (Proxy @n)) - 1

instance (KnownNat n) => Enum (Index n Clamp) where
  toEnum = Index
  fromEnum (Index n) = fromIntegral (max minBound . min maxBound $ n)

instance (KnownNat n) => Enum (Index n Mod) where
  toEnum = Index
  fromEnum (Index n) = fromIntegral (n `mod` modulus)
    where modulus = fromIntegral $ natVal (Proxy @n)

type family ShowIndex (i::Ind) :: Symbol where
  ShowIndex Clamp = "Clamp"
  ShowIndex Mod = "Mod"

coerceIndex :: Index n (i :: Ind) -> Index n (j :: Ind)
coerceIndex = unsafeCoerce

coerceDims :: (KnownNat m, Enum (Index m i)) => Index n i -> Index m i
coerceDims (Index i) = Index i

indexInBounds :: Index n i -> Bool
indexInBounds i@(Index _) = i >= minBound && i <= maxBound
