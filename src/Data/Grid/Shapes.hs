{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Shapes
  (neighbouringWindow, Neighbours(..)) where

import GHC.TypeNats
import Data.Grid.Internal.Grid
import Data.Bifunctor
import Data.Bifunctor.Join
import Data.Bifunctor.Biff
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Grid.Internal.Convolution
import Data.Grid.Internal.Coord
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Coerce
import Data.Function
import Data.Functor.Rep
import Data.Proxy
import Control.Comonad

-- | Simplifies convolutions where the focus is treated separately than the
-- neighbours. Typically the 'Grid' will have a 'Nothing' at the focus.
-- See 'neighbouringWindow'
newtype Neighbours (window :: [Nat]) a = Neighbours (a, (Grid window (Maybe a)))
    deriving (Functor, Applicative, Foldable) via Join (Biff (,) Identity (Compose (Grid window) Maybe))
    deriving Traversable

-- instance Comonad (Neighbours window) where
--   extract (Neighbours (a, _)) = a
--   duplicate (Neighbours (a, g)) = 

-- | A selector for use with 'convolute'. Helpful for situations where the focus of a
-- convolution
neighbouringWindow :: forall window dims.
                   (Neighboring window, Dimensions window)
                   => Coord dims
                   -> Neighbours window (Coord dims)
neighbouringWindow focus = coerce (focus, (window @window @dims focus & imapRep wrapMaybe))
  where
    wrapMaybe (coerceCoordDims -> c) a
      | c == focus = Nothing
      | otherwise = Just a
