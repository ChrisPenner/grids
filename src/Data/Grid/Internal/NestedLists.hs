{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Grid.Internal.NestedLists where

import           GHC.TypeNats            as N
import           Data.Singletons.Prelude
import qualified Data.Vector             as V

-- | Computes the level of nesting requried to represent a given grid
-- dimensionality as a nested list
--
-- > Nested [2, 3] Int == [[Int]]
-- > Nested [2, 3, 4] Int == [[[Int]]]
type family Nested (f :: * -> *) (dims :: [Nat])  a :: * where
  Nested _ '[] a = a
  Nested f (_:xs) a = f (Nested f xs a)

chunkVector :: forall a . Int -> V.Vector a -> [V.Vector a]
chunkVector n v
  | V.null v
  = []
  | otherwise
  = let (before, after) = V.splitAt n v in before : chunkVector n after


-- | Represents valid dimensionalities. All non empty lists of Nats have
-- an instance
class Sizable  (dims :: [Nat]) where
  nestLists :: Proxy dims -> V.Vector a -> Nested [] dims a
  unNestLists :: Proxy dims -> Nested [] dims a -> [a]

  -- | Get the total size of a 'Grid' of the given dimensions
  --
  -- > gridSize (Proxy @'[2, 2]) == 4
  gridSize :: Proxy dims -> Int

instance {-# OVERLAPPING #-} KnownNat x => Sizable '[x] where
  nestLists _ = V.toList
  unNestLists _ xs = xs
  gridSize _ = fromIntegral $ natVal (Proxy @x)

instance {-# OVERLAPPABLE #-} (KnownNat x, Sizable (y:xs)) => Sizable (x:y:xs) where
  nestLists _ v = nestLists (Proxy @(y:xs)) <$> chunkVector (gridSize $ Proxy @(y:xs)) v
  unNestLists _ xs = concat (unNestLists (Proxy @(y:xs)) <$> xs)
  gridSize _ = gridSize (Proxy @(y:xs)) * fromIntegral (natVal (Proxy @x))
