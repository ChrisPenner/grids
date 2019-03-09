{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Grid.Shapes
  (neighbouringWindow, Neighbours(..), focus, neighbours) where

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
import Data.Functor.Rep
import Data.Distributive
import Data.Grid.Internal.Errors
import Data.Maybe

-- | Simplifies convolutions where the focus is treated separately than the
-- neighbours. Typically the 'Grid' will have a 'Nothing' at the focus.
-- See 'neighbouringWindow'
newtype Neighbours (window :: [Nat]) a = Neighbours (a, Compose (Grid window) Maybe a)
    deriving (Functor, Applicative, Foldable) via Join (Biff (,) Identity (Compose (Grid window) Maybe))
    deriving Traversable

focus :: Neighbours window a -> a
focus (Neighbours (a, _)) = a

neighbours :: Neighbours window a -> Compose (Grid window) Maybe a
neighbours (Neighbours (_, n)) = n

instance  (Centered window, Dimensions window) => Distributive (Neighbours window) where
  distribute = distributeRep

instance (Centered window, Dimensions window) => Representable (Neighbours window) where
  type Rep (Neighbours window) = Rep (Grid window)
  index (Neighbours (focus, Compose rest)) c = fromMaybe focus $ index rest c
  tabulate f = Neighbours (f $ centerCoord @window, Compose $ tabulate (Just . f))

class Centered (dims :: [Nat]) where
  centerCoord :: Coord dims

type Even (n :: Nat) = Mod n 2 == 0
type Odd (n :: Nat) = Not (Even n)
type OddC (n :: Nat) = Odd n ?! ShowType n :<>: Text " must be odd"

instance {-# OVERLAPPING #-} (OddC x, KnownNat x) => Centered '[x] where
  centerCoord = Coord [mid]
    where
      mid = (+1) . div 2 . fromIntegral . natVal $ Proxy @x

instance {-# OVERLAPPABLE #-} (OddC x, KnownNat x, Centered xs) => Centered (x:xs) where
  centerCoord = Coord (mid : coerce (centerCoord @xs))
    where
      mid = (+1) . div 2 . fromIntegral . natVal $ Proxy @x


-- instance Comonad (Neighbours window) where
--   extract (Neighbours (a, _)) = a
--   duplicate n@(Neighbours (a, g)) = Neighbours (n, )

-- | A selector for use with 'convolute'. Helpful for situations where the focus of a
-- convolution
neighbouringWindow :: forall window dims.
                   (Neighboring window, Dimensions window, Centered window)
                   => Coord dims
                   -> Neighbours window (Coord dims)
neighbouringWindow focus = coerce (focus, (window @window @dims focus & imapRep wrapMaybe))
  where
    wrapMaybe c a
      | c == centerCoord @window = Nothing
      | otherwise = Just a
