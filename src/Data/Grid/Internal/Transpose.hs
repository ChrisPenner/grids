{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
module Data.Grid.Internal.Transpose where

import Data.Grid.Internal.Grid
import Data.Grid.Internal.Coord
import GHC.TypeNats
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Functor.Rep
import Data.Vector as V

-- class Transposable (key :: [Nat]) from ys where
--   transpose :: Grid from a -> Grid ys a

-- type family Transposed key from where
--   Transposed _ _ = [2, 2]

transpose :: forall (key :: [Nat]) from to a. (SingI key, Indexable from Clamp, Indexable to Clamp) => Grid from a -> Grid to a
transpose g@(Grid v) = Grid newVector'
  where
    transpMap :: Grid from (Coord to Clamp)
    transpMap = tabulate (transposeCoord @key)
    vTranspMap :: V.Vector (Coord to Clamp)
    vTranspMap = toVector transpMap
    newVector :: Vector Int
    newVector = fmap fromEnum vTranspMap
    newVector' :: Vector a
    newVector' = V.generate (V.length v) (\n -> v V.! (newVector V.! n))

transposeCoord :: forall (key :: [Nat]) from ys ind. SingI key => Coord from ind -> Coord ys ind
transposeCoord (Coord cs) = Coord newCoord
 where
  key :: [Int]
  key = fromIntegral <$> demote @key
  newCoord :: [Int]
  newCoord = (key !!) <$> cs
