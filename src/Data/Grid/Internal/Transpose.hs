{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Grid.Internal.Transpose where

import Data.Grid.Internal.Grid
import GHC.TypeNats
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe

class Transposable from to xs ys where
  transpose :: Grid xs a -> Grid ys a

instance Transposable '[n] '[n] '[x] '[x] where
  transpose = id

type family GetNextDim (from :: [Nat]) (to :: [Nat]) (xs :: [Nat]) = (next  :: [Nat]) where
  GetNextDim '[] _ _ = '[]
  GetNextDim (f:fs) ts xs = (xs !! FromJust (ElemIndex f ts)) : GetNextDim fs ts xs
