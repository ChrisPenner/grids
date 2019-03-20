{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Grid.Internal.Shapes
  ( partitionFocus
  , centerCoord
  , centeredCoords
  , Centered
  ) where

import GHC.TypeNats
import Data.Grid.Internal.Grid
import Data.Grid.Internal.Coord
import Data.Singletons.Prelude
import Data.Coerce
import Data.Functor.Rep
import Data.Grid.Internal.Errors

partitionFocus :: forall window a.
               (Centered window, IsGrid window)
               => Grid window a
               -> (a, Grid window (Maybe a))
partitionFocus g = (g `index` centerCoord @window, imapRep wrapMaybe g)
  where
    wrapMaybe c a
      | c == centerCoord @window = Nothing
      | otherwise = Just a


type Even (n :: Nat) = Mod n 2 == 0

type Odd (n :: Nat) = Not (Even n)

type OddC (n :: Nat) =
  Odd n ?! 'Text "Dimension '"
           ':<>: 'ShowType n
           ':<>: 'Text " must be ODD to use 'center' based functions"

class Centered (dims :: [Nat]) where
  centerCoord :: Coord dims

instance {-# OVERLAPPING #-} (OddC x, KnownNat x) => Centered '[x] where
  centerCoord = Coord [mid]
    where
      mid = (fromIntegral . natVal $ Proxy @x) `div` 2

instance {-# OVERLAPPABLE #-} (OddC x, KnownNat x, Centered xs) => Centered (x:xs) where
  centerCoord = Coord (mid : coerce (centerCoord @xs))
    where
      mid = (fromIntegral . natVal $ Proxy @x) `div` 2

centeredCoords :: forall dims. (Centered dims, IsGrid dims) => Grid dims (Coord dims)
centeredCoords = tabulate adjustCoord
    where
        adjustCoord :: Coord dims -> Coord dims
        adjustCoord (Coord cs) = Coord $ zipWith subtract (unCoord $ centerCoord @dims) cs
