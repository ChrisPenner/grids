{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Internal.Nest where

import Data.Grid.Internal.Types
import Data.Grid.Internal.Coord
import Data.Singletons.Prelude
import Data.Maybe

joinGrid :: Grid dims (Grid ns a) -> Grid (dims ++ ns) a
joinGrid (Grid v) = Grid (v >>= toVector)

splitGrid
  :: forall ind dims ns a
   . ( Dimensions (dims ++ ns)
     , Dimensions ns
     , Dimensions dims
     , NestedLists (dims ++ ns) a ~ NestedLists dims (NestedLists ns a)
     )
  => Grid (dims ++ ns) a
  -> Grid dims (Grid ns a)
splitGrid = fmap fromNestedLists' . fromNestedLists' . toNestedLists
