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
import           GHC.TypeLits                  as TL
import           Data.List.Singletons  as L
import           Data.Maybe.Singletons
import           Data.Functor.Rep
import           Data.Vector                   as V
import           Prelude.Singletons

type family Permuted (key :: [Nat]) (from :: [Nat]) :: [Nat] where
  Permuted '[] _ = '[]
  Permuted (x:xs) from = (from !! x) : Permuted xs from

type ValidPermutation key from =
  (Sort key == EnumFromTo 0 (Length from TL.- 1)) ?!
    ('Text "Malformed permutation hint: " ':<>: 'ShowType key
                ':$$: 'Text "When permuting matrix of size: " ':<>: 'ShowType from
                ':$$: 'Text "Key must be a permutation of "  ':<>: 'ShowType (EnumFromTo 0 (Length from TL.- 1))
                ':$$: 'Text "e.g. the identity permutation for 2x2 is @[0, 1]"
                ':$$: 'Text "e.g. matrix transpose for 2x2 is @[1, 0]"
  )

-- | Permute dimensions of a 'Grid'. This is similar to MatLab's permute
-- function
--
-- 'permute' requires a type application containing a permutation pattern;
-- The pattern is a re-ordering of the list @[0..n]@ which represents the new
-- dimension order. For example the permutation pattern @[1, 2, 0]@ when
-- applied to the dimensions @[4, 5, 6]@ results in the dimensions @[5, 6, 4]@.
--
-- For 2 dimensional matrixes, a permutation using @[1, 0]@ is simply a 
-- matrix 'transpose'
--
-- > λ> small
-- > fromNestedLists
-- >   [[0,1,2]
-- >   ,[3,4,5]
-- >   ,[6,7,8]]
-- >
-- > λ> permute @[1, 0] small
-- > fromNestedLists
-- >   [[0,3,6]
-- >   ,[1,4,7]
-- >   ,[2,5,8]]
permute
  :: forall (key :: [Nat]) from a invertedKey
   . ( SingI invertedKey
     , invertedKey ~ InvertKey (EnumFromTo 0 (Length from TL.- 1)) key
     , ValidPermutation key from
     , IsGrid from 
     , IsGrid (Permuted key from) 
     )
  => Grid from a
  -> Grid (Permuted key from) a
permute (Grid v) = result
 where
  result :: Grid (Permuted key from) a
  result = tabulate
    ((v V.!) . fromEnum  . permuteCoord @invertedKey @from)

-- | Permute the dimensions of a coordinate according to a permutation pattern.
-- see 'permute' regarding permutation patterns
permuteCoord
  :: forall (key :: [Nat]) to from 
   . (SingI key)
  => Coord from 
  -> Coord to 
permuteCoord (Coord cs) = Coord newCoord
 where
  key :: [Int]
  key = fromIntegral <$> demote @key
  newCoord :: [Int]
  newCoord = (cs !!) <$> key

-- | Transpose a 2 dimensional matrix. Equivalent to:
--
-- > permute @[1, 0]
transpose :: (IsGrid '[x, y], IsGrid '[y, x]) => Grid '[x, y] a -> Grid '[y, x] a
transpose = permute @'[1, 0]

-- | Get the inverse of a permutation pattern, used internally
type family InvertKey ref key :: [Nat] where
  InvertKey '[] xs = '[]
  InvertKey (n:ns) xs = FromJust (ElemIndex n xs) : InvertKey ns xs
