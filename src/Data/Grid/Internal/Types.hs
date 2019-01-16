{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.Grid.Internal.Types
  ( Grid(..)
  , GridSize
  , Dimensions(..)
  , Coord
  , NestedLists
  , generate
  , toNestedLists
  , fromNestedLists
  , fromList
  , (//)
  )
where

import Data.Grid.Internal.Coord
import           Data.Distributive
import           Data.Functor.Rep
import qualified Data.Vector                   as V
import           Data.Proxy
import           Data.Kind
import           GHC.TypeNats                  as N
import           Data.Finite
import           Control.Applicative
import           Data.List
import           Data.Bifunctor

toFinite :: (KnownNat n) => Integral m => m -> Finite n
toFinite = finite . fromIntegral

fromFinite :: Num n => Finite m -> n
fromFinite = fromIntegral . getFinite

-- | An grid of arbitrary dimensions.
--
-- e.g. a @Grid [2, 3] Int@ might look like:
--
-- > generate id :: Grid [2, 3] Int
-- > (Grid [[0,1,2],
-- >        [3,4,5]])
newtype Grid ind (dims :: [Nat]) a =
  Grid  (V.Vector a)
  deriving (Eq, Functor, Foldable, Traversable)

instance (Dimensions dims, Show (NestedLists dims a)) => Show (Grid ind dims a) where
  show g = "(Grid " ++ show (toNestedLists g) ++ ")"

instance (Dimensions dims, AsCoord (Coord ind dims) dims, Semigroup a) => Semigroup (Grid ind dims a) where
  (<>) = liftA2 (<>)

instance (Dimensions dims, AsCoord (Coord ind dims) dims, Monoid a) => Monoid (Grid ind dims a) where
  mempty = pure mempty

instance (Dimensions dims, AsCoord (Coord ind dims) dims) => Applicative (Grid ind dims) where
  pure a = tabulate (const a)
  liftA2 f (Grid v) (Grid u) = Grid $ V.zipWith f v u

-- | Calculate the number of elements in a grid of the given dimensionality
type family GridSize (dims :: [Nat]) :: Nat where
  GridSize '[] = 0
  GridSize (x:'[]) = x
  GridSize (x:xs) = (x N.* GridSize xs)

-- | Represents valid dimensionalities. All non empty lists of Nats have
-- instances
class (AllC KnownNat dims, KnownNat (GridSize dims)) => Dimensions (dims :: [Nat]) where
  -- toCoord :: Proxy dims -> Finite (GridSize dims) -> Coord dims
  -- fromCoord :: Proxy dims -> Coord dims -> Finite (GridSize dims)
  gridSize
    :: Proxy dims -> Int
  gridSize _ = fromIntegral $ natVal (Proxy @(GridSize dims))
  nestLists :: Proxy dims -> V.Vector a -> NestedLists dims a
  unNestLists :: Proxy dims -> NestedLists dims a -> [a]

type family AllC (c :: x -> Constraint) (ts :: [x]) :: Constraint where
  AllC c '[] = ()
  AllC c (x:xs) = (c x, AllC c xs)

instance (KnownNat x) => Dimensions '[x] where
  -- toCoord _ i = i
  -- fromCoord _ i = i
  nestLists _ = V.toList
  unNestLists _ xs = xs

instance (KnownNat (GridSize (x:y:xs)), KnownNat x, Dimensions (y:xs)) => Dimensions (x:y:xs) where
  -- toCoord _ n = firstCoord :# toCoord (Proxy @(y:xs)) remainder
  --   where
  --     firstCoord = toFinite (n `div` fromIntegral (gridSize (Proxy @(y:xs))))
  --     remainder = toFinite (fromFinite n `mod` gridSize (Proxy @(y:xs)))
  -- fromCoord _ (x :# ys) =
  --   toFinite $ firstPart + rest
  --     where
  --       firstPart = fromFinite x * gridSize (Proxy @(y:xs))
  --       rest = fromFinite (fromCoord (Proxy @(y:xs)) ys)
  nestLists _ v = nestLists (Proxy @(y:xs)) <$> chunkVector (Proxy @(GridSize (y:xs))) v
  unNestLists _ xs = concat (unNestLists (Proxy @(y:xs)) <$> xs)

instance (Dimensions dims, AsCoord (Coord ind dims) dims) => Distributive (Grid ind dims) where
  distribute = distributeRep

instance (Dimensions dims, AsCoord (Coord ind dims) dims) => Representable (Grid ind dims) where
  type Rep (Grid ind dims) = Coord ind dims
  index (Grid v) ind = v V.! fromCoord (Proxy @dims) ind
  tabulate f = Grid $ V.generate (fromIntegral $ gridSize (Proxy @dims)) (f . toCoord (Proxy @dims) . fromIntegral)

-- | Computes the level of nesting requried to represent a given grid
-- dimensionality as a nested list
--
-- > NestedLists [2, 3] Int == [[Int]]
-- > NestedLists [2, 3, 4] Int == [[[Int]]]
type family NestedLists (dims :: [Nat]) a where
  NestedLists '[] a = a
  NestedLists (_:xs) a = [NestedLists xs a]

-- | Build a grid by selecting an element for each element
generate :: forall ind dims a . Dimensions dims => (Int -> a) -> Grid ind dims a
generate f = Grid $ V.generate (gridSize (Proxy @dims)) f

chunkVector :: forall n a . KnownNat n => Proxy n -> V.Vector a -> [V.Vector a]
chunkVector _ v
  | V.null v
  = []
  | otherwise
  = let (before, after) = V.splitAt (fromIntegral $ natVal (Proxy @n)) v
    in  before : chunkVector (Proxy @n) after

-- | Turn a grid into a nested list structure. List nesting increases for each
-- dimension
--
-- > toNestedLists (G.generate id :: Grid [2, 3] Int)
-- > [[0,1,2],[3,4,5]]
toNestedLists
  :: forall ind dims a
   . (Dimensions dims)
  => Grid ind dims a
  -> NestedLists dims a
toNestedLists (Grid v) = nestLists (Proxy @dims) v

-- | Turn a nested list structure into a Grid if the list is well formed. 
-- Required list nesting increases for each dimension
--
-- > fromNestedLists [[0,1,2],[3,4,5]] :: Maybe (Grid [2, 3] Int)
-- > Just (Grid [[0,1,2],[3,4,5]])
-- > fromNestedLists [[0],[1,2]] :: Maybe (Grid [2, 3] Int)
-- > Nothing
fromNestedLists
  :: forall ind dims a
   . Dimensions dims
  => NestedLists dims a
  -> Maybe (Grid ind dims a)
fromNestedLists = fromList . unNestLists (Proxy @dims)

-- | Convert a list into a Grid or fail if not provided the correct number of
-- elements
--
-- > G.fromList [0, 1, 2, 3, 4, 5] :: Maybe (Grid [2, 3] Int)
-- > Just (Grid [[0,1,2],[3,4,5]])
-- > G.fromList [0, 1, 2, 3] :: Maybe (Grid [2, 3] Int)
-- > Nothing
fromList
  :: forall a ind dims
   . (KnownNat (GridSize dims), Dimensions dims)
  => [a]
  -> Maybe (Grid ind dims a)
fromList xs =
  let v = V.fromList xs
  in  if V.length v == gridSize (Proxy @dims) then Just $ Grid v else Nothing

-- | Update elements of a grid
(//)
  :: forall ind dims a
   . (Dimensions dims, AsCoord (Coord ind dims) dims)
  => Grid ind dims a
  -> [(Coord ind dims, a)]
  -> Grid ind dims a
(Grid v) // xs = Grid (v V.// fmap (first (fromCoord (Proxy @dims))) xs)
