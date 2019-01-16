{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Grid.Internal.Dims where

import Data.Proxy
import GHC.TypeNats

class Sizeable d where
  gridSize :: Proxy d -> Int

instance (KnownNat n) => Sizeable (n :: Nat) where
  gridSize _ = fromIntegral $ natVal (Proxy @n)

instance (KnownNat x, Sizeable xs) => Sizeable (x:xs :: [Nat]) where
  gridSize _ = fromIntegral (natVal (Proxy @x)) * gridSize (Proxy @xs)

instance Sizeable '[] where
  gridSize _ = 1
