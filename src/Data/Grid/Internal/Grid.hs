{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Internal.Grid
  ( Grid(..)
  , IsGrid
  , Coord
  , NestedLists
  , generate
  , toNestedLists
  , fromNestedLists
  , fromNestedLists'
  , fromList
  , fromList'
  , (//)

  , Neighboring(..)

  , joinGrid
  , splitGrid
  )
where

import Data.Kind
import           Data.Grid.Internal.NestedLists
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Pretty
import           Data.Distributive
import           Data.Functor.Rep
import qualified Data.Vector                    as V
import           Data.Proxy
import           GHC.TypeNats                   as N hiding (Mod)
import           Control.Applicative
import           Data.Bifunctor
import           Data.Maybe
import           Control.DeepSeq
import           Prelude.Singletons

type family AllC (c :: x -> Constraint) (ts :: [x]) :: Constraint where
  AllC c '[] = ()
  AllC c (x:xs) = (c x, AllC c xs)


type IsGrid dims =
  ( AllC KnownNat dims
  , SingI dims
  , Sizable dims
  , Representable (Grid dims)
  , Enum (Coord dims)
  , Bounded (Coord dims)
  , Neighboring dims
  )

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

instance (PrettyList (NestedLists dims a), IsGrid dims, Show (NestedLists dims a)) => Show (Grid dims a) where
  show g = "fromNestedLists \n" ++ (unlines . fmap ("  " ++ ) . lines $ prettyList (toNestedLists g))

instance (IsGrid dims, Semigroup a) => Semigroup (Grid dims a) where
  (<>) = liftA2 (<>)

instance (IsGrid dims, Monoid a) => Monoid (Grid dims a) where
  mempty = pure mempty

instance (IsGrid dims) => Applicative (Grid dims) where
  pure a = tabulate (const a)
  liftA2 f (Grid v) (Grid u) = Grid $ V.zipWith f v u

instance (IsGrid dims) => Distributive (Grid dims) where
  distribute = distributeRep

instance (IsGrid dims) => Representable (Grid dims) where
  type Rep (Grid dims) = Coord dims
  index (Grid v) c = v V.! fromEnum c
  tabulate f = Grid $ V.generate (fromIntegral $ gridSize (Proxy @dims)) (f . toEnum  . fromIntegral)

instance (Num n, IsGrid dims) => Num (Grid dims n) where
  (+)  = liftA2 (+)
  (*)  = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

-- | Build a grid by selecting an element for each element
generate :: forall dims a . (IsGrid dims) => (Int -> a) -> Grid dims a
generate f = Grid $ V.generate (gridSize $ Proxy @dims) f

-- | Turn a grid into a nested list structure. List nesting increases for each
-- dimension
--
-- > toNestedLists (G.generate id :: Grid [2, 3] Int)
-- > [[0,1,2],[3,4,5]]
toNestedLists
  :: forall dims a . (IsGrid dims) => Grid dims a -> NestedLists dims a
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
   . IsGrid dims
  => NestedLists dims a
  -> Maybe (Grid dims a)
fromNestedLists = fromList . unNestLists (Proxy @dims)

-- | Partial variant of 'fromNestedLists' which errors on malformed input
fromNestedLists'
  :: forall dims a . IsGrid dims => NestedLists dims a -> Grid dims a
fromNestedLists' = fromJust . fromNestedLists

-- | Convert a list into a Grid or fail if not provided the correct number of
-- elements
--
-- > G.fromList [0, 1, 2, 3, 4, 5] :: Maybe (Grid [2, 3] Int)
-- > Just (Grid [[0,1,2],[3,4,5]])
-- > G.fromList [0, 1, 2, 3] :: Maybe (Grid [2, 3] Int)
-- > Nothing
fromList :: forall dims a . (IsGrid dims) => [a] -> Maybe (Grid dims a)
fromList xs =
  let v = V.fromList xs
  in  if V.length v == gridSize (Proxy @dims) then Just $ Grid v else Nothing

-- | Partial variant of 'fromList' which errors on malformed input
fromList' :: forall dims a . (IsGrid dims) => [a] -> Grid dims a
fromList' = fromJust . fromList

-- | Update elements of a grid
(//)
  :: forall dims a
   . IsGrid dims
  => Grid dims a
  -> [(Coord dims , a)]
  -> Grid dims a
(Grid v) // xs = Grid (v V.// fmap (first fromEnum) xs)

class Neighboring dims where
  neighborCoords :: Grid dims (Coord dims)


instance {-# OVERLAPPING #-} (IsGrid '[n]) => Neighboring '[n]  where
  neighborCoords = fromList' . fmap (Coord . pure . subtract (numVals `div` 2)) . take numVals $ [0 .. ]
    where
      numVals = gridSize (Proxy @'[n])

instance (KnownNat n, Neighboring ns) => Neighboring (n:ns) where
  neighborCoords = joinGrid (addCoord <$> currentLevelNeighbors)
    where
      addCoord :: Coord '[n]  -> Grid ns (Coord (n : ns) )
      addCoord c = appendC c <$> nestedNeighbors
      nestedNeighbors :: Grid ns (Coord ns )
      nestedNeighbors = neighborCoords
      currentLevelNeighbors :: Grid '[n] (Coord '[n] )
      currentLevelNeighbors = neighborCoords


-- | The inverse of 'splitGrid', 
-- joinGrid will nest a grid from:
-- > Grid outer (Grid inner a) -> Grid (outer ++ inner) a
--
-- For example, you can nest a simple 3x3 from smaller [3] grids as follows:
--
-- > joinGrid (myGrid :: Grid [3] (Grid [3] a)) :: Grid '[3, 3] a
joinGrid :: Grid dims (Grid ns a) -> Grid (dims ++ ns) a
joinGrid (Grid v) = Grid (v >>= toVector)

-- | The inverse of 'joinGrid', 
-- splitGrid @outerDims @innerDims will un-nest a grid from:
-- > Grid (outer ++ inner) a -> Grid outer (Grid inner a)
--
-- For example, you can unnest a simple 3x3 as follows:
--
-- > splitGrid @'[3] @'[3] myGrid :: Grid '[3] (Grid [3] a)
splitGrid :: forall outer inner a from.
          ( IsGrid from
          , IsGrid inner
          , IsGrid outer
          , NestedLists from a ~ NestedLists outer (NestedLists inner a)
          )
          => Grid from a
          -> Grid outer (Grid inner a)
splitGrid = fmap fromNestedLists' . fromNestedLists' . toNestedLists


