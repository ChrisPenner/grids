{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Grid.Internal.NestedLists where

import           Data.Kind
import           GHC.TypeNats                  as N
import           Data.Singletons.Prelude
import qualified Data.Vector                   as V
import           Data.Grid.Internal.Coord

type family AllC (c :: x -> Constraint) (ts :: [x]) :: Constraint where
  AllC c '[] = ()
  AllC c (x:xs) = (c x, AllC c xs)

-- | Computes the level of nesting requried to represent a given grid
-- dimensionality as a nested list
--
-- > NestedLists [2, 3] Int == [[Int]]
-- > NestedLists [2, 3, 4] Int == [[[Int]]]
type family NestedLists (dims :: [Nat]) a where
  NestedLists '[] a = a
  NestedLists (_:xs) a = [NestedLists xs a]

chunkVector :: forall a . Int -> V.Vector a -> [V.Vector a]
chunkVector n v
  | V.null v
  = []
  | otherwise
  = let (before, after) = V.splitAt n v in before : chunkVector n after


-- | Represents valid dimensionalities. All non empty lists of Nats have
-- an instance
class (AllC KnownNat dims, SingI dims, Enum (Coord dims), Bounded (Coord dims)) => Dimensions  (dims :: [Nat]) where
  nestLists :: Proxy dims -> V.Vector a -> NestedLists dims a
  unNestLists :: Proxy dims -> NestedLists dims a -> [a]

instance (KnownNat x) => Dimensions '[x] where
  nestLists _ = V.toList
  unNestLists _ xs = xs

instance (KnownNat x, Bounded (Coord xs), SingI xs, Dimensions (y:xs)) => Dimensions (x:y:xs) where
  nestLists _ v = nestLists (Proxy @(y:xs)) <$> chunkVector (gridSize @(y:xs)) v
  unNestLists _ xs = concat (unNestLists (Proxy @(y:xs)) <$> xs)
