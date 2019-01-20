{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Grid.Internal.Transpose where

import           Data.Grid.Internal.Grid
import           Data.Grid.Internal.Errors
import           Data.Grid.Internal.Coord
import           GHC.TypeNats
import           GHC.TypeLits as TL
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  as L
import           Data.Singletons.Prelude.Maybe
import           Data.Functor.Rep
import           Data.Vector                   as V
import           Data.Kind

type family Transposed (key :: [Nat]) (from :: [Nat]) :: [Nat] where
  Transposed '[] _ = '[]
  Transposed (x:xs) from = (from !! x) : Transposed xs from

type ValidTransposition key from =
  (Sort key == EnumFromTo 0 (Length from TL.- 1)) ?!
    (Text "Malformed transposition hint: " :<>: ShowType key
                :$$: Text "When transposing matrix of size: " :<>: ShowType from
                :$$: Text "Key must be a permutation of "  :<>: ShowType (EnumFromTo 0 (Length from TL.- 1))
                :$$: Text "e.g. the identity transpose for 2x2 is @[0, 1]"
                :$$: Text "e.g. standard transposition for 2x2 is @[1, 0]"
  )

transpose
  :: forall (key :: [Nat]) from a
   . ( SingI key
     , ValidTransposition key from
     , Indexable from Clamp
     , Indexable (Transposed key from) Clamp
     )
  => Grid from a
  -> Grid (Transposed key from) a
transpose (Grid v) = Grid transposedValues
 where
  transpMap :: Grid from (Coord (Transposed key from) Clamp)
  transpMap = tabulate (transposeCoord @key)
  transposedPositions :: Vector Int
  transposedPositions = fromEnum <$> (toVector transpMap)
  transposedValues :: Vector a
  transposedValues =
    V.generate (V.length v) (\n -> v V.! (transposedPositions V.! n))

transposeCoord
  :: forall (key :: [Nat]) from ys ind
   . SingI key
  => Coord from ind
  -> Coord ys ind
transposeCoord (Coord cs) = Coord newCoord
 where
  key :: [Int]
  key = fromIntegral <$> demote @key
  newCoord :: [Int]
  newCoord = (cs !!) <$> key
