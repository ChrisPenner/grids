{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Grid.Examples.Intro where

import Data.Grid
import Data.Maybe
import Data.Functor.Compose
import Data.Coerce
import Data.Foldable
import Data.Functor.Rep
import GHC.TypeNats hiding ()

simpleGrid :: Grid '[5, 5] Int
simpleGrid = generate id

coordGrid :: Grid '[5, 5] (Coord '[5, 5])
coordGrid = tabulate id


avg :: Foldable f => f Int -> Int
avg f | null f    = 0
      | otherwise = sum f `div` length f

mx :: Foldable f => f Int -> Int
mx = maximum

small :: Grid '[3, 3] Int
small = generate id

small' :: Grid '[5, 5] Int
small' = generate id


med :: Grid '[3, 3, 3] Int
med = generate id

big :: Grid '[5, 5, 5, 5] Int
big = generate id

gauss :: (Dimensions dims) => Grid dims Double -> Grid dims Double
gauss = autoConvolute safeWindow gauss'
 where
  gauss' :: Compose (Grid '[3, 3]) Maybe Double -> Double
  gauss' g = (sum g) / fromIntegral (length g)

clampGauss :: (Dimensions dims) => Grid dims Double -> Grid dims Double
clampGauss = autoConvolute clampWindow gauss'
 where
  gauss' :: Grid '[3, 3] Double -> Double
  gauss' g = sum g / fromIntegral (length g)


seeNeighboring :: Grid '[3, 3] a -> Grid '[3, 3] (Grid '[3, 3] (Maybe a))
seeNeighboring = autoConvolute safeWindow go
 where
  go :: Compose (Grid '[3, 3]) Maybe a -> Grid '[3, 3] (Maybe a)
  go = getCompose . coerce

coords :: Grid '[3, 3] (Coord '[3, 3])
coords = tabulate id

doubleGrid :: Grid '[3, 3] Double
doubleGrid = fromIntegral <$> small

simpleGauss :: Grid '[3, 3] Double
simpleGauss = gauss doubleGrid

pacmanGauss :: (Dimensions dims) => Grid dims Double -> Grid dims Double
pacmanGauss = autoConvolute @'[3, 3] wrapWindow gauss'
  where gauss' g = sum g / fromIntegral (length g)
