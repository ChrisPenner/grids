{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
  :: forall ind dims a
   . (Dimensions dims, Enum (Coord ind dims))
  => Coord ind dims
  -> Lens' (Grid ind dims a) a
cell c = lens get set
 where
  get          = flip R.index c
  vectorOffset = fromEnum c
  set (Grid v) new = Grid (v V.// [(vectorOffset, new)])
