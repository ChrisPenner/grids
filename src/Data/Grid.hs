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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Grid (
  Grid(..)
    , GridSize
    , Coord
    , (:#)(..)
    , Dimensions(..)
    , generate
    , NestLists(..)
    , UnNestLists(..)
    , fromNestedLists
    , fromList
    , (//)
    ) where

import           Data.Distributive
import           Data.Functor.Rep
import qualified Data.Vector                   as V
import           GHC.TypeLits                  as L
import           Data.Proxy
import           Data.Functor.Compose
import           Control.Lens
import           Data.Kind
import           GHC.TypeNats                  as N
import           Data.Finite
import           Control.Applicative
import           Data.List
import           Data.Bifunctor
import GHC.Natural
import Data.Singletons.Prelude
import Data.Reflection
import Data.Void

newtype Grid (dims :: [Nat]) a =
  Grid  (V.Vector a)
  deriving (Eq, Functor, Foldable, Traversable)

instance (NestLists dims, Show (NestedLists dims a)) => Show (Grid dims a) where
  show g = "(Grid " ++ show (toNestedLists g) ++ ")"

instance (Dimensions dims, Semigroup a) => Semigroup (Grid dims a) where
  (<>) = liftA2 (<>)

instance (Dimensions dims, Monoid a) => Monoid (Grid dims a) where
  mempty = pure mempty

instance (Dimensions dims) => Applicative (Grid dims) where
  pure a = tabulate (const a)
  liftA2 f (Grid v) (Grid u) = Grid $ V.zipWith f v u

type family GridSize (dims :: [Nat]) :: Nat where
  GridSize '[] = 1
  GridSize (x:'[]) = x
  GridSize (x:xs) = (x N.* GridSize xs)

data x :# y = x :# y
  deriving (Show, Eq, Ord)

infixr 9 :#

type family Coord (dims :: [Nat]) where
  Coord '[] = Void
  Coord '[n] = Finite n
  Coord (n:xs) = Finite n :# Coord xs

class (AllC KnownNat dims, KnownNat (GridSize dims)) => Dimensions (dims :: [Nat]) where
  toCoord :: Proxy dims -> Finite (GridSize dims) -> Coord dims
  fromCoord :: Proxy dims -> Coord dims -> Finite (GridSize dims)
  gridSize
    :: Proxy dims -> Int
  gridSize _ = fromIntegral $ L.natVal (Proxy @(GridSize dims))

type family AllC (c :: x -> Constraint) (ts :: [x]) :: Constraint where
  AllC c '[] = ()
  AllC c (x:xs) = (c x, AllC c xs)

instance Dimensions '[] where
  toCoord _ _ = error "Empty grid has no members"
  fromCoord _ v = absurd v

instance (KnownNat x) => Dimensions '[x] where
  toCoord _ i = i
  fromCoord _ i = i

instance (KnownNat (GridSize (x:y:xs)), KnownNat x, Dimensions (y:xs)) => Dimensions (x:y:xs) where
  toCoord _ n = firstCoord :# toCoord (Proxy @(y:xs)) remainder
    where
      firstCoord = toFinite (n `div` fromIntegral (gridSize (Proxy @(y:xs))))
      remainder = toFinite (fromFinite n `mod` gridSize (Proxy @(y:xs)))
  fromCoord _ (x :# ys) =
    toFinite $ firstPart + rest
      where
        firstPart = fromFinite x * gridSize (Proxy @(y:xs))
        rest = fromFinite (fromCoord (Proxy @(y:xs)) ys)

toFinite :: (KnownNat n) => Integral m => m -> Finite n
toFinite = finite . fromIntegral

fromFinite :: Num n => Finite m -> n
fromFinite = fromIntegral . getFinite

instance (Dimensions dims) => Distributive (Grid dims) where
  distribute = distributeRep

instance (Dimensions dims) => Representable (Grid dims) where
  type Rep (Grid dims) = Coord dims
  index (Grid v) ind = v V.! fromIntegral (fromCoord (Proxy @dims) ind)
  tabulate f = Grid $ V.generate (fromIntegral $ gridSize (Proxy @dims)) (f . toCoord (Proxy @dims) . fromIntegral)

instance (Dimensions dims, ind ~ Coord dims)
  => FunctorWithIndex ind (Grid dims) where
    imap = imapRep

instance (Dimensions dims, ind ~ Coord dims)
  => FoldableWithIndex ind (Grid dims) where
    ifoldMap = ifoldMapRep

instance (Dimensions dims, ind ~ Coord dims)
  => TraversableWithIndex ind (Grid dims) where
    itraverse = itraverseRep

generate :: forall dims a . Dimensions dims => (Int -> a) -> Grid dims a
generate f = Grid $ V.generate (gridSize (Proxy @dims)) f

type family NestedLists (dims :: [Nat]) a where
  NestedLists '[] a = a
  NestedLists (_:xs) a = [NestedLists xs a]

class NestLists (dims :: [Nat]) where
  nestLists :: Proxy dims -> V.Vector a -> NestedLists dims a

chunkVector :: forall n a . KnownNat n => Proxy n -> V.Vector a -> [V.Vector a]
chunkVector _ v
  | V.null v
  = []
  | otherwise
  = let (before, after) = V.splitAt (fromIntegral $ L.natVal (Proxy @n)) v
    in  before : chunkVector (Proxy @n) after

instance (KnownNat n) => NestLists '[n] where
  nestLists _ = V.toList

instance (KnownNat n, NestLists (n:ns), Dimensions (m:n:ns), Dimensions (n:ns)) => NestLists (m:n:ns) where
  nestLists _ v = nestLists (Proxy @(n:ns)) <$> chunkVector (Proxy @(GridSize (n:ns))) v

toNestedLists
  :: forall dims a . (NestLists dims) => Grid dims a -> NestedLists dims a
toNestedLists (Grid v) = nestLists (Proxy @dims) v

class UnNestLists (dims :: [Nat]) where
  unNestLists :: Proxy dims -> NestedLists dims a -> [a]

instance UnNestLists '[n] where
  unNestLists _ xs = xs

instance (UnNestLists (n:ns)) => UnNestLists (m:n:ns) where
  unNestLists _ xs = concat (unNestLists (Proxy @(n:ns)) <$> xs)

fromNestedLists
  :: forall dims a
   . (UnNestLists dims, Dimensions dims)
  => NestedLists dims a
  -> Maybe (Grid dims a)
fromNestedLists = fromList . unNestLists (Proxy @dims)

fromList
  :: forall a dims
   . (KnownNat (GridSize dims), Dimensions dims)
  => [a]
  -> Maybe (Grid dims a)
fromList xs =
  let v = V.fromList xs
  in  if V.length v == gridSize (Proxy @dims) then Just $ Grid v else Nothing

(//)
  :: forall dims a
   . (Dimensions dims)
  => Grid dims a
  -> [(Coord dims, a)]
  -> Grid dims a
(Grid v) // xs =
  Grid (v V.// fmap (first (fromFinite . fromCoord (Proxy @dims))) xs)


-- class ToGrid (k :: [Nat]) where
  -- toGrid :: forall k. Grid k ()

-- existentializeGrid
--   :: forall r
--    . [Integer]
--   -> (forall d . (Show (Grid d ()), Dimensions d) => Grid d () -> r)
--   -> r
-- existentializeGrid [] f = error "dimensions must not be empty"
-- existentializeGrid xs f = helper xs
--  where
--   helper :: [Integer] -> r
--   helper (x : xs) = reifyNat x (recurse xs)
--   recurse :: forall n . KnownNat n => [Integer] -> Proxy n -> r
--   recurse [] _ = f (pure () :: Grid '[n] ())
--     -- recurse xs = 


-- withGrid :: forall r . [Integer] -> (forall d . Dimensions d => Grid d () -> r) -> r
-- withGrid dimensions f = stuff
--  where
--   stuff :: r
--   stuff =
--     (withSomeSing :: Demote [Nat]
--         -> (forall d . Dimensions d => Sing (d :: [Nat]) -> r)
--         -> r
--       )
--       (fromIntegral <$> dimensions :: [Natural])
--       go
--   go :: forall dims . Dimensions dims => Sing (dims :: [Nat]) -> r
--   go _ = f (pure () :: Grid dims ())

-- tabulate' :: forall d a . [Int] -> a -> Grid d a
-- tabulate' dimensions x = undefined
--  where
--   stuff = withSomeSing (fromIntegral <$> dimensions) go
--   go :: Sing (x :: [Nat]) -> Grid d a
--   go _ = pure x
