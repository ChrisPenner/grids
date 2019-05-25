{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Internal.Ix where

import Data.Proxy
import GHC.TypeNats
import qualified Data.Massiv.Array as A
import Data.Singletons.Prelude.List

class ToIndex dims where
    sizeAsIndex :: A.Ix (Length dims)

instance ToIndex '[] where
    sizeAsIndex = A.Ix0

intNat :: forall n. KnownNat n => Int
intNat = fromIntegral (natVal $ Proxy @n)

instance KnownNat n => ToIndex '[n] where
    sizeAsIndex = intNat @n - 1

instance (KnownNat x, KnownNat y)  => ToIndex (x:y:'[]) where
    sizeAsIndex = intNat @x - 1 A.:. intNat @y - 1

instance {-# OVERLAPPABLE #-}( A.Ix n ~ A.IxN n
                             , KnownNat x
                             , ToIndex ys
                             , ixRest ~ A.Ix (Length ys)
                             , n ~ Length (x : ys)
                             , ixRest ~ A.Ix (n - 1)
                             ) => ToIndex (x : ys) where
    sizeAsIndex = intNat @x - 1 A.:> (sizeAsIndex @ys :: ixRest) :: A.Ix n
