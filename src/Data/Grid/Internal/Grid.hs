{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Internal.Grid
  ( Grid(..)
  , Dimensions(..)
  , Coord
  , NestedLists
  , generate
  , toNestedLists
  , fromNestedLists
  , fromNestedLists'
  , fromList
  , fromList'
  , (//)
  )
where

import           Data.Grid.Internal.NestedLists
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Pretty
import           Data.Distributive
import           Data.Functor.Rep
import qualified Data.Vector                   as V
import           Data.Proxy
import           GHC.TypeNats                  as N
                                                   hiding ( Mod )
import           Control.Applicative
import           Data.Bifunctor
import           Data.Maybe
import           Data.Singletons.Prelude
import           Control.DeepSeq

-- | An grid of arbitrary dimensions.
--
-- e.g. a @Grid [2, 3] Int@ might look like:
--
-- > generate id :: Grid [2, 3] Int
-- > fromNestedLists [[0,1,2],
-- >                  [3,4,5]]
newtype Grid (dims :: [Nat]) a =
  Grid  {toVector :: V.Vector a}
  deriving (Eq, Functor, Foldable, Traversable, NFData)

instance (PrettyList (NestedLists dims a), Dimensions dims, Show (NestedLists dims a)) => Show (Grid dims a) where
  show g = "fromNestedLists \n" ++ (unlines . fmap ("  " ++ ) . lines $ prettyList (toNestedLists g))

instance (Dimensions dims, Semigroup a) => Semigroup (Grid dims a) where
  (<>) = liftA2 (<>)

instance (Dimensions dims, Monoid a) => Monoid (Grid dims a) where
  mempty = pure mempty

instance (Dimensions dims) => Applicative (Grid dims) where
  pure a = tabulate (const a)
  liftA2 f (Grid v) (Grid u) = Grid $ V.zipWith f v u

instance (Dimensions dims) => Distributive (Grid dims) where
  distribute = distributeRep

instance (Dimensions dims) => Representable (Grid dims) where
  type Rep (Grid dims) = Coord dims
  index (Grid v) c = v V.! fromEnum c
  tabulate f = Grid $ V.generate (fromIntegral $ gridSize @dims) (f . toEnum  . fromIntegral)

instance (Num n, Dimensions dims) => Num (Grid dims n) where
  (+)  = liftA2 (+)
  (*)  = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

-- | Build a grid by selecting an element for each element
generate :: forall dims a . (SingI dims) => (Int -> a) -> Grid dims a
generate f = Grid $ V.generate (gridSize @dims) f

-- | Turn a grid into a nested list structure. List nesting increases for each
-- dimension
--
-- > toNestedLists (G.generate id :: Grid [2, 3] Int)
-- > [[0,1,2],[3,4,5]]
toNestedLists
  :: forall dims a . (Dimensions dims) => Grid dims a -> NestedLists dims a
toNestedLists (Grid v) = nestLists (Proxy @dims) v

-- | Turn a nested list structure into a Grid if the list is well formed. 
-- Required list nesting increases for each dimension
--
-- > fromNestedLists [[0,1,2],[3,4,5]] :: Maybe (Grid [2, 3] Int)
-- > Just (Grid [[0,1,2],[3,4,5]])
-- > fromNestedLists [[0],[1,2]] :: Maybe (Grid [2, 3] Int)
-- > Nothing
fromNestedLists
  :: forall dims a
   . Dimensions dims
  => NestedLists dims a
  -> Maybe (Grid dims a)
fromNestedLists = fromList . unNestLists (Proxy @dims)

-- | Partial variant of 'fromNestedLists' which errors on malformed input
fromNestedLists'
  :: forall dims a . Dimensions dims => NestedLists dims a -> Grid dims a
fromNestedLists' = fromJust . fromNestedLists

-- | Convert a list into a Grid or fail if not provided the correct number of
-- elements
--
-- > G.fromList [0, 1, 2, 3, 4, 5] :: Maybe (Grid [2, 3] Int)
-- > Just (Grid [[0,1,2],[3,4,5]])
-- > G.fromList [0, 1, 2, 3] :: Maybe (Grid [2, 3] Int)
-- > Nothing
fromList :: forall dims a . (SingI dims) => [a] -> Maybe (Grid dims a)
fromList xs =
  let v = V.fromList xs
  in  if V.length v == gridSize @dims then Just $ Grid v else Nothing

-- | Partial variant of 'fromList' which errors on malformed input
fromList' :: forall dims a . (SingI dims) => [a] -> Grid dims a
fromList' = fromJust . fromList

-- | Update elements of a grid
(//)
  :: forall dims a
   . (Enum (Coord dims ))
  => Grid dims a
  -> [(Coord dims , a)]
  -> Grid dims a
(Grid v) // xs = Grid (v V.// fmap (first fromEnum) xs)
