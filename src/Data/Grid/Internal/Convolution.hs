{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveTraversable #-}


module Data.Grid.Internal.Convolution
  ( autoConvolute
  , convolute
  , clampBounds
  , wrapBounds
  , omitBounds
  , window
  , usingStore
  , Neighboring
  ) where

import           Control.Comonad
import           Control.Comonad.Representable.Store
import           Data.Functor.Compose
import           Data.Functor.Rep
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Grid

-- | Perform a computation based on the context surrounding a cell
-- Good for doing things like Linear Image Filters (e.g. gaussian blur) or
-- simulating Cellular Automata (e.g. Conway's game of life)
--
-- This function accepts a function which indicates what to do with
-- 'out-of-bounds' indexes, 'clampWindow', 'wrapWindow' and 'safeWindow'
-- are examples.
--
-- It also acccepts a transformation function which operates over the
-- functor created by the first parameter and collapses it down to a new
-- value for the cell at that position.
--
-- This function is best used with Type Applications to denote the desired
-- window size; the Grid passed to the given function contains the current cell
-- (in the middle) and all the surrounding cells.
--
-- Here's an example of computing the average of all neighboring cells,
-- repeating values at the edge of the grid when indexes are out of bounds
-- (using 'clampWindow')
--
-- > gaussian :: (IsGrid dims) => Grid dims Double -> Grid dims Double
-- > gaussian = autoConvolute clampBounds avg
-- >  where
-- >   avg :: Grid '[3, 3] Double -> Double
-- >   avg g = sum g / fromIntegral (length g)
autoConvolute
  :: forall window dims f a b
   . ( IsGrid dims
     , IsGrid window
     , Functor f
     )
  => (Grid window (Coord dims) -> f (Coord dims)) -- ^ Restrict out of bounds coordinates in some way. Use 'clampWindow', 'wrapWindow' or 'safeWindow'
  -> (f a -> b) -- ^ Collapse the context down to a value
  -> Grid dims a -- ^ Starting grid
  -> Grid dims b
autoConvolute restrict = convolute (restrict . window @window @dims)

-- | This is a fully generic version of 'autoConvolute' which allows
-- the user to provide a function which builds a context from the current
-- coord, then provides a collapsing function over the same functor.
convolute
  :: forall dims f a b
   . (Functor f, IsGrid dims)
  => (Coord dims -> f (Coord dims))  -- ^ Build a neighboring context within a functor from the current coord
  -> (f a -> b) -- ^ Collapse the context to a single value
  -> Grid dims a -- ^ Starting grid
  -> Grid dims b
convolute selectWindow f g =
  let conv :: Store (Grid dims) a -> Store (Grid dims) b
      conv     = extend (f . experiment (fmap roundTrip . selectWindow))
   in usingStore g conv
 where
  roundTrip :: Coord dims -> Coord dims
  roundTrip = toEnum . fromEnum

-- | Given a coordinate generate a grid of size 'window' filled with
-- coordinates surrounding the given coord. Mostly used internally
window
  :: forall window dims
   . (IsGrid window)
  => Coord dims
  -> Grid window (Coord dims)
window = fromWindow . neighboring . toWindow
 where
  toWindow :: Coord dims -> Coord window
  toWindow = coerceCoordDims
  fromWindow :: Grid window (Coord window) -> Grid window (Coord dims)
  fromWindow = fmap coerceCoordDims


neighboring :: (IsGrid dims) => Coord dims -> Grid dims (Coord dims)
neighboring c = (c +) <$> neighborCoords

-- | Use with 'autoConvolute'; Clamp out-of-bounds coordinates to the nearest in-bounds coord.
clampBounds
  :: (IsGrid dims, Functor f) => f (Coord dims) -> f (Coord dims)
clampBounds = fmap clampCoord

-- | Use with 'autoConvolute'; Wrap out-of-bounds coordinates pac-man style to the other side of the grid
wrapBounds
  :: (IsGrid dims, Functor f) => f (Coord dims) -> f (Coord dims)
wrapBounds = fmap wrapCoord

-- | Use with 'autoConvolute'; Out of bounds coords become 'Nothing'
omitBounds
  :: (IsGrid dims, Functor f) => f (Coord dims) -> Compose f Maybe (Coord dims)
omitBounds = Compose . fmap wrap
  where
    wrap c | coordInBounds c = Just c
           | otherwise  = Nothing

-- | Perform an operation over the 'Store' representation of a Grid
usingStore :: (IsGrid dims, IsGrid dims')
           => Grid dims a
           -> (Store (Grid dims) a -> Store (Grid dims') b)
           -> Grid dims' b
usingStore g f = tabulate . fst . runStore . f $ store (index g) (toEnum 0)
