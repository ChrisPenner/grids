{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Grid.Lens (cell) where

import Data.Grid
import Data.Functor.Rep as R
import Data.Vector as V
import Data.Proxy

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

-- | Focus an element of a grid
cell
  :: forall dims a. (Dimensions dims, Eq (Coord dims)) => Coord dims -> Lens' (Grid dims a) a
cell c = lens get set 
  where 
    get = flip R.index c
    vectorOffset = fromIntegral (fromCoord (Proxy @dims) c)
    set (Grid v) new = Grid (v V.// [(vectorOffset, new)])
