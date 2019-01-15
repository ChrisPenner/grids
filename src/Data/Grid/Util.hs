{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Grid.Util where

import Data.Grid
import Data.Functor.Rep
import GHC.TypeNats
import Data.Kind
import Control.Applicative

import Control.Comonad.Representable.Store

convolute
  :: IsSubgrid window dims
  => (Grid window (Maybe a) -> b)
  -> Grid dims a
  -> Grid dims b
convolute f g = undefined

neighbouring
  :: forall dims window
   . (Dimensions window, IsSubgrid window dims)
  => Coord dims
  -> Grid window (Coord dims)
neighbouring c = tabulate go
 where
  go :: Coord window -> Coord dims
  go w = undefined -- collapse c

type family IsSubgrid (child :: [Nat]) (parent :: [Nat]):: Constraint where
  IsSubgrid '[] '[] = ()
  IsSubgrid '[child] '[parent] = child <= parent
  IsSubgrid (child:cs) (parent:ps) = (child <= parent, IsSubgrid cs ps)

class (Applicative t) => TraverseCoord t c d where
  sequenceC :: c -> t d

instance (TraverseCoord t y d') => TraverseCoord t (t x :# y) (x :# d') where
  sequenceC (tx :# y) = (:#) <$> tx <*> sequenceC y

type family HeadC xs where
  HeadC (x :# _) = x
  HeadC x = x

class Collapsable c where
  collapse :: c -> [Int]
  expand :: [Int] -> c

class (Applicative f) => TraverseHappy c f where
  traversal :: (forall n. KnownNat n => Finite n -> f (Finite n)) -> c -> f c

instance (TraverseHappy xs f, KnownNat n) => TraverseHappy (Finite n :# xs) f where
  traversal f (x :# xs) = liftA2 (:#) (go x) (traversal f xs)
    where
      go :: Finite n -> f (Finite n)
      go x = f x

instance {-# OVERLAPPABLE #-} (KnownNat n, Applicative f) => TraverseHappy (Finite n) f where
  traversal f x = f x

instance {-# OVERLAPPABLE #-} (Integral x) => Collapsable x where
  collapse = pure . fromIntegral
  expand [] = error "not enough values to expand"
  expand [x] = fromIntegral x
  expand _ = error "too many values to expand"

instance (Num x, Collapsable x, Collapsable xs) => Collapsable (x :# xs) where
  collapse (x :# xs) = collapse x ++ collapse xs
  expand (x:xs) = fromIntegral x :# expand xs
  expand _ = error "not enough values to expand"

adjust :: (KnownNat n) => (Integer -> Integer) -> Finite n -> Maybe (Finite n)
adjust f = packFinite . f . fromIntegral

-- adjustCoord :: (Int -> Int) -> 
