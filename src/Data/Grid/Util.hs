{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Grid.Util where

import Data.Grid
import Data.Functor.Rep
import GHC.TypeNats
import Data.Kind
import Control.Applicative
import Data.Functor.Compose

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Maybe

convolute
  :: forall f ind dims a b
   . (Functor f, Dimensions dims, AsCoord (Coord ind dims) dims)
  => (Coord ind dims -> f (Coord ind dims))
  -> (f a -> b)
  -> Grid ind dims a
  -> Grid ind dims b
convolute selectWindow f g =
  let s = store (index g) undefined
      convoluted :: Store (Grid ind dims) b
      convoluted     = extend (f . experiment selectWindow) s
      (tabulator, _) = runStore convoluted
  in  tabulate tabulator

threeByThree :: (Num x, Num y) => Grid ind '[3, 3] (x :# y)
threeByThree = fromJust $ fromNestedLists
  [ [(-1) :# (-1), (-1) :# 0, (-1) :# 1]
  , [0 :# (-1), 0 :# 0, 0 :# 1]
  , [1 :# (-1), 1 :# 0, 1 :# 1]
  ]

threeByThree' :: (Num x, Num y) => (x :# y) -> Grid ind '[3, 3] (x :# y)
threeByThree' = traverse (+) threeByThree


data Orth a =
  Orth
    { up :: a
    , right :: a
    , down :: a
    , left :: a
    } deriving (Eq, Show, Functor, Traversable, Foldable)

orthNeighbours
  :: (KnownNat x, KnownNat y)
  => (Tagged x :# Tagged y)
  -> Compose Orth Maybe (Tagged x :# Tagged y)
orthNeighbours c = Compose
  (   toMaybe
  <$> traverse
        (+)
        Orth {up = 0 :# (-1), right = 1 :# 0, down = 0 :# 1, left = -1 :# 0}
        c
  )
 where
  toMaybe c@(x :# y) | not (inBounds x) || not (inBounds y) = Nothing
                     | otherwise                            = Just c

avg :: Foldable f => f Int -> Int
avg f | null f    = 0
      | otherwise = sum f `div` length f

mx :: Foldable f => f Int -> Int
mx = maximum

small :: Grid C '[5, 5] Int
small = generate id

med :: Grid T '[5, 5, 5] Int
med = generate id

big :: Grid T '[5, 5, 5, 5] Int
big = generate id


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

-- class (Applicative f) => TraverseHappy c f where
--   traversal :: (forall n. KnownNat n => Finite n -> f (Finite n)) -> c -> f c

-- instance (TraverseHappy xs f, KnownNat n) => TraverseHappy (Finite n :# xs) f where
--   traversal f (x :# xs) = liftA2 (:#) (f x) (traversal f xs)

-- instance {-# OVERLAPPABLE #-} (KnownNat n, Applicative f) => TraverseHappy (Finite n) f where
--   traversal f x = f x

-- instance {-# OVERLAPPABLE #-} (Integral x) => Collapsable x where
--   collapse = pure . fromIntegral
--   expand [] = error "not enough values to expand"
--   expand [x] = fromIntegral x
--   expand _ = error "too many values to expand"

-- instance (Num x, Collapsable x, Collapsable xs) => Collapsable (x :# xs) where
--   collapse (x :# xs) = collapse x ++ collapse xs
--   expand (x:xs) = fromIntegral x :# expand xs
--   expand _ = error "not enough values to expand"

-- adjust :: (KnownNat n) => (Integer -> Integer) -> Finite n -> Maybe (Finite n)
-- adjust f = packFinite . f . fromIntegral

-- adjustCoord :: (Int -> Int) -> 
