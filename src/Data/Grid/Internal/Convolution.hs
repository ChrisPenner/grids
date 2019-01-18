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
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Grid.Internal.Convolution where

-- import Data.Grid.Internal.Index
-- import Data.Grid.Internal.Types
-- import Data.Grid.Internal.Coord
-- import Data.Grid.Internal.Nest
-- import Data.Grid.Internal.Tagged
-- import Data.Functor.Rep
-- import GHC.TypeNats
-- import Data.Kind
-- import Control.Applicative
-- import Data.Functor.Compose
-- import Data.Foldable
-- import Data.Coerce

-- import Control.Comonad
-- import Control.Comonad.Representable.Store
-- import Data.Maybe
-- import Data.Proxy

-- criticalError :: a
-- criticalError = error
--   "Something went wrong, please report this issue to the maintainer of grids"

-- autoConvolute
--   :: forall window dims ind a b
--    . (Dimensions dims, Neighboring (Coord window ind) (Grid ind window))
--   => (Grid ind window a -> b)
--   -> Grid ind dims a
--   -> Grid ind dims b
-- autoConvolute = convolute (window @window @dims)

-- gconvolute
--   :: forall dims ind f a b
--    . (Functor f, Dimensions dims, Enum (Coord dims ind))
--   => (Coord dims ind -> f (Coord dims ind))
--   -> (f a -> b)
--   -> Grid ind dims a
--   -> Grid ind dims b
-- gconvolute selectWindow f g =
--   let s = store (index g) criticalError
--       convoluted :: Store (Grid ind dims) b
--       convoluted     = extend (f . experiment selectWindow) s
--       (tabulator, _) = runStore convoluted
--   in  tabulate tabulator

-- convolute
--   :: forall window dims ind a b
--    . (Dimensions dims, Enum (Coord dims ind))
--   => (Coord dims ind -> Grid ind window (Coord dims ind))
--   -> (Grid ind window a -> b)
--   -> Grid ind dims a
--   -> Grid ind dims b
-- convolute selectWindow f g = gconvolute selectWindow f g

-- safeConvolute
--   :: forall window dims ind a b
--    . (Dimensions dims)
--   => (Coord dims ind -> Grid ind window (Coord dims ind))
--   -> (Grid ind window (Maybe a) -> b)
--   -> Grid ind dims a
--   -> Grid ind dims b
-- safeConvolute selectWindow f = gconvolute (restrict . selectWindow)
--                                           (f . getCompose)
--  where
--   restrict
--     :: Grid ind window (Coord dims ind)
--     -> Compose (Grid ind window) Maybe (Coord dims ind)
--   restrict = Compose . fmap go
--    where
--     go b | inBounds b = Just b
--          | otherwise  = Nothing

-- safeAutoConvolute
--   :: forall window dims ind a b
--    . ( Dimensions dims
--      , Neighboring (Coord window ind) (Grid ind window)
--      , Coercible (Coord dims ind) (Coord window ind)
--      )
--   => (Grid ind window (Maybe a) -> b)
--   -> Grid ind dims a
--   -> Grid ind dims b
-- safeAutoConvolute = safeConvolute (window @window @dims)

-- window
--   :: forall window dims ind
--    . ( Coercible (Coord dims ind) (Coord window ind)
--      , Neighboring (Coord window ind) (Grid ind window)
--      )
--   => Coord dims ind
--   -> Grid ind window (Coord dims ind)
-- window = fromWindow . neighboring . toWindow
--  where
--   toWindow :: Coord dims ind -> Coord window ind
--   toWindow = coerce
--   fromWindow
--     :: Grid ind window (Coord window ind) -> Grid ind window (Coord dims ind)
--   fromWindow = coerce

-- data Orth a =
--   Orth
--     { up :: a
--     , right :: a
--     , down :: a
--     , left :: a
--     } deriving (Eq, Show, Functor, Traversable, Foldable)

-- orthNeighbours :: Coord dims ind -> Compose Orth Maybe (Coord dims ind)
-- orthNeighbours c = Compose
--   (   toMaybe
--   <$> traverse
--         (+)
--         Orth {up = 0 :# (-1), right = 1 :# 0, down = 0 :# 1, left = -1 :# 0}
--         c
--   )
--  where
--   toMaybe c@(x :# y) | not (inBounds x) || not (inBounds y) = Nothing
--                      | otherwise                            = Just c

-- orthFromList [up', right', down', left'] =
--   Orth {up = up, right = right', down = down', left = left'}

-- class Neighboring c g where
--   neighbors :: g c

-- instance {-# OVERLAPPING #-} Neighboring c (Grid ind '[n]) where
--   neighbors = fromList' . fmap toEnum . fmap (subtract (numVals `div` 2)) . take numVals $ [0 .. ]
--     where
--       numVals = inhabitants (Proxy @c)

-- instance (Neighboring x (Grid ind '[n]), Neighboring y (Grid ind ns)) => Neighboring (Coord dims ind) (Grid ind (n:ns)) where
--   neighbors = joinGrid (addCoord <$> currentLevelNeighbors)
--     where
--       -- addCoord :: (Index n ind) -> Grid ind ns (Coord ns ind)
--       addCoord x = (x :#) <$> nestedNeighbors
--       nestedNeighbors :: Grid ind ns y
--       nestedNeighbors = neighbors
--       currentLevelNeighbors :: Grid ind '[n] x
--       currentLevelNeighbors = neighbors

-- neighboring :: (Num c, Neighboring c (Grid ind dims)) => c -> Grid ind dims c
-- neighboring c = (c +) <$> neighbors


-- -- instance {-# OVERLAPPABLE #-} (Integral x) => Collapsable x where
-- --   collapse = pure . fromIntegral
-- --   expand [] = error "not enough values to expand"
-- --   expand [x] = fromIntegral x
-- --   expand _ = error "too many values to expand"

-- -- instance (Num x, Collapsable x, Collapsable xs) => Collapsable (x :# xs) where
-- --   collapse (x :# xs) = collapse x ++ collapse xs
-- --   expand (x:xs) = fromIntegral x :# expand xs
-- --   expand _ = error "not enough values to expand"
