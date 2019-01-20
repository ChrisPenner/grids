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
import           Data.Maybe
import Data.List 

type family Permuted (key :: [Nat]) (from :: [Nat]) :: [Nat] where
  Permuted '[] _ = '[]
  Permuted (x:xs) from = (from !! x) : Permuted xs from

type ValidPermutation key from =
  (Sort key == EnumFromTo 0 (Length from TL.- 1)) ?!
    (Text "Malformed permutation hint: " :<>: ShowType key
                :$$: Text "When permuting matrix of size: " :<>: ShowType from
                :$$: Text "Key must be a permutation of "  :<>: ShowType (EnumFromTo 0 (Length from TL.- 1))
                :$$: Text "e.g. the identity permutation for 2x2 is @[0, 1]"
                :$$: Text "e.g. matrix transpose for 2x2 is @[1, 0]"
  )

--transposeSlow
--  :: forall (key :: [Nat]) from a
--   . ( SingI key
--     , ValidPermutation key from
--     , Indexable from Clamp
--     , Indexable (Permuted key from) Clamp
--     )
--  => Grid from a
--  -> Grid (Permuted key from) a
--transposeSlow (Grid v) = Grid done
-- where
--  len = V.length v
--  transpMap :: V.Vector Int
--  transpMap = V.generate len (fromEnum . transposeCoord @key . toEnum @(Coord from Clamp))
--  zipped :: V.Vector (Int, Int)
--  zipped = indexed transpMap
--  sorted = sortOn snd (toList zipped)
--  collapsed = fmap fst sorted
--  collected = fmap (v V.!) collapsed
--  done = V.fromList collected
--  -- This is a really slow way to do this
--  -- transposedValues :: V.Vector a
--  -- transposedValues = V.generate len (\i -> transpMap V.! i)

-- This is more confusing but maybe faster??
permute
  :: forall (key :: [Nat]) from a invertedKey
   . ( SingI invertedKey
     , invertedKey ~ InvertKey (EnumFromTo 0 (Length from TL.- 1)) key
     , ValidPermutation key from
     , Indexable from Clamp
     , Indexable (Permuted key from) Clamp
     )
  => Grid from a
  -> Grid (Permuted key from) a
permute (Grid v) = result
 where
  len = V.length v
  result :: Grid (Permuted key from) a
  result = tabulate ((v V.!) . fromEnum @(Coord from Clamp) . permuteCoord @invertedKey @from)

permuteCoord
  :: forall (key :: [Nat]) to from ind
   . (SingI key)
  => Coord from ind
  -> Coord to ind
permuteCoord (Coord cs) = Coord newCoord
 where
  key :: [Int]
  key = fromIntegral <$> demote @key
  newCoord :: [Int]
  newCoord = (cs !!) <$> key

transpose :: (Dimensions [x, y]) => Grid [x, y] a -> Grid [y, x] a
transpose = permute @[1, 0]

type family InvertKey ref key :: [Nat] where
  InvertKey '[] xs = '[]
  InvertKey (n:ns) xs = FromJust (ElemIndex n xs) : InvertKey ns xs
