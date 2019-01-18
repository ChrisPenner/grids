{-# LANGUAGE PolyKinds #-}
module Data.Grid.Examples.Intro where

-- import Data.Grid
-- import Data.Maybe
-- import Data.Functor.Compose
-- import Data.Coerce
-- import Data.Foldable
-- import Data.Functor.Rep
-- import GHC.TypeNats

-- simpleGrid :: Grid L '[5, 5] Int
-- simpleGrid = generate id

-- modGrid :: Grid M '[5, 5] Int
-- modGrid = generate id

-- clampedGrid :: Grid C '[2, 2, 2, 2, 2, 2, 2] Int
-- clampedGrid = generate id

-- avg :: Foldable f => f Int -> Int
-- avg f | null f    = 0
--       | otherwise = sum f `div` length f

-- mx :: Foldable f => f Int -> Int
-- mx = maximum

-- small :: Grid C '[3, 3] Int
-- small = generate id

-- med :: Grid C '[3, 3, 3] Int
-- med = generate id

-- big :: Grid C '[5, 5, 5, 5] Int
-- big = generate id

-- threeByThree :: (Num x, Num y) => Grid ind '[3, 3] (x :# y)
-- threeByThree = fromJust $ fromNestedLists
--   [ [(-1) :# (-1), (-1) :# 0, (-1) :# 1]
--   , [0 :# (-1), 0 :# 0, 0 :# 1]
--   , [1 :# (-1), 1 :# 0, 1 :# 1]
--   ]

-- threeByThree'
--   :: (Num (x :# y), Num x, Num y) => (x :# y) -> Grid ind '[3, 3] (x :# y)
-- threeByThree' = traverse (+) threeByThree

-- gauss
--   :: ( Coercible (Coord ind '[3, 3]) (Coord ind dims)
--      , Dimensions dims
--      , Index (Coord ind dims)
--      , Index (Coord ind '[3, 3])
--      , Neighboring (Coord ind '[3, 3]) (Grid ind '[3, 3])
--      )
--   => Grid (ind :: Ind) dims Double
--   -> Grid ind dims Double
-- gauss = safeAutoConvolute gauss'
--  where
--   gauss' :: Grid ind '[3, 3] (Maybe Double) -> Double
--   gauss' g = (sum . Compose $ g) / fromIntegral (length (Compose g))

-- seeNeighboring :: Grid C '[3, 3] a -> Grid C '[3, 3] (Grid C '[3, 3] (Maybe a))
-- seeNeighboring = safeAutoConvolute go
--  where
--   go :: Grid C '[3, 3] (Maybe a) -> Grid C '[3, 3] (Maybe a)
--   go = coerce

-- coords :: Grid C '[3, 3] (Clamp 3 :# Clamp 3)
-- coords = tabulate id

-- simpleGauss :: Grid C '[3, 3] Double
-- simpleGauss = gauss (fromIntegral <$> small)

-- myGauss :: Grid C '[9, 9] Double -> Grid C '[9, 9] Double
-- myGauss = safeAutoConvolute @'[3, 3] gauss'
--   where
--   -- gauss' :: Grid ind '[3, 3] (Maybe Double) -> Double
--         gauss' g = (sum . Compose $ g) / fromIntegral (length (Compose g))
