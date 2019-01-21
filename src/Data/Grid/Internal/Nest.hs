{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Grid.Internal.Nest where

import Data.Grid.Internal.Grid
import Data.Grid.Internal.Coord
import Data.Singletons.Prelude
import Data.Maybe


-- | The inverse of 'splitGrid', 
-- joinGrid will nest a grid from:
-- > Grid outer (Grid inner a) -> Grid (outer ++ inner) a
--
-- For example, you can nest a simple 3x3 from smaller [3] grids as follows:
--
-- > joinGrid (myGrid :: Grid [3] (Grid [3] a)) :: Grid '[3, 3] a
joinGrid :: Grid dims (Grid ns a) -> Grid (dims ++ ns) a
joinGrid (Grid v) = Grid (v >>= toVector)

-- | The inverse of 'joinGrid', 
-- splitGrid @outerDims @innerDims will un-nest a grid from:
-- > Grid (outer ++ inner) a -> Grid outer (Grid inner a)
--
-- For example, you can unnest a simple 3x3 as follows:
--
-- > splitGrid @'[3] @'[3] myGrid :: Grid '[3] (Grid [3] a)
splitGrid
  :: forall outer inner a from
   . ( from ~ (outer ++ inner)
     , Dimensions from
     , Dimensions inner
     , Dimensions outer
     , NestedLists from a ~ NestedLists outer (NestedLists inner a)
     )
  => Grid from a
  -> Grid outer (Grid inner a)
splitGrid = fmap fromNestedLists' . fromNestedLists' . toNestedLists
