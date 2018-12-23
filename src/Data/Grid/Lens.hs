{-# LANGUAGE RankNTypes #-}
module Data.Grid.Lens where

import Data.SafeGrid
import Control.Lens as L
import Data.Functor.Rep as R

cell
  :: (Dimensions dims, Eq (Coords dims)) => Coords dims -> Lens' (Grid dims a) a
cell c = lens (`R.index` c) (\s b -> s & itraversed . L.index c .~ b)
