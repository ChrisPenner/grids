{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Grid.Internal.Convolution where

import           Data.Grid.Internal.Grid
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Nest
import           Data.Functor.Rep
import           GHC.TypeNats
import           Data.Kind
import           Control.Applicative
import           Data.Functor.Compose
import           Data.Foldable
import           Data.Coerce

import           Control.Comonad
import           Control.Comonad.Representable.Store
import           Data.Maybe
import           Data.Proxy

criticalError :: a
criticalError = error
  "Something went wrong, please report this issue to the maintainer of grids"

autoConvolute
  :: forall window dims f a b
   . ( Dimensions dims
     , Dimensions window
     , Functor f
     , Neighboring (Coord window) (Grid window)
     )
  => (Grid window (Coord dims) -> f (Coord dims))
  -> (f a -> b)
  -> Grid dims a
  -> Grid dims b
autoConvolute restrict = gconvolute (restrict . window @window @dims)

gconvolute
  :: forall dims f a b
   . (Functor f, Dimensions dims)
  => (Coord dims -> f (Coord dims))
  -> (f a -> b)
  -> Grid dims a
  -> Grid dims b
gconvolute selectWindow f g =
  let s = store (index g) criticalError
      convoluted :: Store (Grid dims) b
      convoluted     = extend (f . experiment (fmap roundTrip . selectWindow)) s
      (tabulator, _) = runStore convoluted
  in  tabulate tabulator
 where
  roundTrip :: Coord dims -> Coord dims
  roundTrip = toEnum . fromEnum

convolute
  :: forall window dims a b
   . (Dimensions dims)
  => (Coord dims -> Grid window (Coord dims))
  -> (Grid window a -> b)
  -> Grid dims a
  -> Grid dims b
convolute = gconvolute

safeConvolute
  :: forall window dims a b
   . (Dimensions dims)
  => (Coord dims -> Grid window (Coord dims))
  -> (Grid window (Maybe a) -> b)
  -> Grid dims a
  -> Grid dims b
safeConvolute selectWindow f = gconvolute (restrict . selectWindow)
                                          (f . getCompose)
 where
  restrict
    :: Grid window (Coord dims) -> Compose (Grid window) Maybe (Coord dims)
  restrict = Compose . fmap go
   where
    go b | coordInBounds b = Just b
         | otherwise       = Nothing

safeAutoConvolute
  :: forall window dims a b
   . ( Dimensions dims
     , Dimensions window
     , Neighboring (Coord window) (Grid window)
     )
  => (Grid window (Maybe a) -> b)
  -> Grid dims a
  -> Grid dims b
safeAutoConvolute = safeConvolute window

window
  :: forall window dims
   . (Neighboring (Coord window) (Grid window), Num (Coord window))
  => Coord dims
  -> Grid window (Coord dims)
window = fromWindow . neighboring . toWindow
 where
  toWindow :: Coord dims -> Coord window
  toWindow = coerceCoordDims
  fromWindow :: Grid window (Coord window) -> Grid window (Coord dims)
  fromWindow = fmap coerceCoordDims

-- data Orth a =
--   Orth
--     { up :: a
--     , right :: a
--     , down :: a
--     , left :: a
--     } deriving (Eq, Show, Functor, Traversable, Foldable)

-- orthNeighbours :: Coord dims  -> Compose Orth Maybe (Coord dims )
-- orthNeighbours c = Compose
--   (   toMaybe
--   <$> traverse
--         (+)
--         Orth {up = 0 :# (-1), right = 1 :# 0, down = 0 :# 1, left = -1 :# 0}
--         c
--   )
--  where
--   toMaybe c@(x :# y) | not (inBounds x) || not (inBounds y) = Nothing
--                      | otherwise                            = Just c

-- orthFromList [up', right', down', left'] =
--   Orth {up = up, right = right', down = down', left = left'}

class Neighboring c g where
  neighbors :: g c

instance {-# OVERLAPPING #-} (KnownNat n) => Neighboring (Coord '[n] ) (Grid '[n]) where
  neighbors = fromList' . fmap (Coord . pure . subtract (numVals `div` 2)) . take numVals $ [0 .. ]
    where
      numVals = inhabitants @'[n]

instance (KnownNat n, Neighboring (Coord ns ) (Grid ns)) => Neighboring (Coord (n:ns) ) (Grid (n:ns)) where
  neighbors = joinGrid (addCoord <$> currentLevelNeighbors)
    where
      addCoord :: Coord '[n]  -> Grid ns (Coord (n : ns) )
      addCoord c = (appendC c) <$> nestedNeighbors
      nestedNeighbors :: Grid ns (Coord ns )
      nestedNeighbors = neighbors
      currentLevelNeighbors :: Grid '[n] (Coord '[n] )
      currentLevelNeighbors = neighbors

neighboring :: (Num c, Neighboring c (Grid dims)) => c -> Grid dims c
neighboring c = (c +) <$> neighbors

clampWindow :: (Dimensions dims) => Grid window (Coord dims) -> Grid window (Coord dims)
clampWindow = fmap clampCoord

wrapWindow :: (Dimensions dims) => Grid window (Coord dims) -> Grid window (Coord dims)
wrapWindow = fmap wrapCoord
