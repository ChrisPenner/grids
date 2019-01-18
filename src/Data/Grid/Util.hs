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
{-# LANGUAGE PolyKinds #-}
module Data.Grid.Util where

import Data.Grid.Internal.Types
import Data.Grid.Internal.Coord
import Data.Grid.Internal.Nest
import Data.Grid.Internal.Tagged
import Data.Functor.Rep
import GHC.TypeNats
import Data.Kind
import Control.Applicative
import Data.Functor.Compose
import Data.Foldable
import Data.Coerce

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Maybe
import Data.Proxy

criticalError :: a
criticalError = error
  "Something went wrong, please report this issue to the maintainer of grids"

autoConvolute
  :: forall window dims ind a b
   . ( Dimensions dims
     , Enum (Coord ind dims)
     , Coercible (Coord ind window) (Coord ind dims)
     , Neighboring (Coord ind window) (Grid ind window)
     , (Num (Coord ind window))
     )
  => (Grid ind window a -> b)
  -> Grid ind dims a
  -> Grid ind dims b
autoConvolute = convolute (fromWindow . neighboring . toWindow)
 where
  toWindow :: Coord ind dims -> Coord ind window
  toWindow = coerce
  fromWindow
    :: Grid ind window (Coord ind window) -> Grid ind window (Coord ind dims)
  fromWindow = coerce

convolute
  :: forall f ind dims a b
   . (Functor f, Dimensions dims, Enum (Coord ind dims))
  => (Coord ind dims -> f (Coord ind dims))
  -> (f a -> b)
  -> Grid ind dims a
  -> Grid ind dims b
convolute selectWindow f g =
  let s = store (index g) criticalError
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

threeByThree'
  :: (Num (x :# y), Num x, Num y) => (x :# y) -> Grid ind '[3, 3] (x :# y)
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

small :: Grid C '[3, 3] Int
small = generate id

med :: Grid C '[3, 3, 3] Int
med = generate id

big :: Grid C '[5, 5, 5, 5] Int
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

class Neighboring c g where
  neighbors :: g c

instance {-# OVERLAPPING #-} (AsIndex c n) => Neighboring c (Grid ind '[n]) where
  neighbors = fromList' . fmap toEnum . fmap (subtract (numVals `div` 2)) . take numVals $ [0 .. ]
    where
      numVals = inhabitants (Proxy @c)

instance (Neighboring x (Grid ind '[n]), Neighboring y (Grid ind ns)) => Neighboring (x :# y) (Grid ind (n:ns)) where
  neighbors = joinGrid (addCoord <$> currentLevelNeighbors)
    where
      addCoord :: x -> Grid ind ns (x :# y)
      addCoord x = (x :#) <$> nestedNeighbors
      nestedNeighbors :: Grid ind ns y
      nestedNeighbors = neighbors
      currentLevelNeighbors :: Grid ind '[n] x
      currentLevelNeighbors = neighbors

neighboring :: (Num c, Neighboring c (Grid ind dims)) => c -> Grid ind dims c
neighboring c = (c +) <$> neighbors


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

-- class Surround c where
--   surrounding :: Int -> c -> [ c ]

-- instance (Enum c) => Surround c where
--   surrounding n c = (fromEnum c `div` 2)
--     where
--       mid = fromEnum c
