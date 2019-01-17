{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Internal.Nest where

import Data.Grid.Internal.Types
import Data.Grid.Internal.Coord
import Data.Singletons.Prelude
import Data.Maybe

joinGrid :: Grid ind dims (Grid ind ns a) -> Grid ind (dims ++ ns) a
joinGrid (Grid v) = Grid (v >>= toVector)

splitGrid
  :: forall ind dims ns a
   . ( Dimensions (dims ++ ns)
     , Dimensions ns
     , Dimensions dims
     , NestedLists (dims ++ ns) a ~ NestedLists dims (NestedLists ns a)
     )
  => Grid ind (dims ++ ns) a
  -> Grid ind dims (Grid ind ns a)
splitGrid = fmap fromNestedLists' . fromNestedLists' . toNestedLists
