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
import GHC.TypeNats hiding (Mod)

simpleGrid :: Grid '[5, 5] Int
simpleGrid = generate id

coordGrid :: Grid '[5, 5] (Coord '[5, 5] Clamp)
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

gauss :: (Indexable dims) => Grid dims Double -> Grid dims Double
gauss = safeAutoConvolute gauss'
 where
  gauss' :: Grid '[3, 3] (Maybe Double) -> Double
  gauss' g = (sum . Compose $ g) / fromIntegral (length (Compose g))

seeNeighboring :: Grid '[3, 3] a -> Grid '[3, 3] (Grid '[3, 3] (Maybe a))
seeNeighboring = safeAutoConvolute go
 where
  go :: Grid '[3, 3] (Maybe a) -> Grid '[3, 3] (Maybe a)
  go = coerce

coords :: Grid '[3, 3] (Coord '[3, 3] Clamp)
coords = tabulate id

doubleGrid :: Grid '[3, 3] Double
doubleGrid = fromIntegral <$> small

simpleGauss :: Grid '[3, 3] Double
simpleGauss = gauss doubleGrid

myGauss :: Grid '[9, 9] Double -> Grid '[9, 9] Double
myGauss = safeAutoConvolute @'[3, 3] gauss'
  where gauss' g = (sum . Compose $ g) / fromIntegral (length (Compose g))

pacmanGauss
  :: (Dimensions dims, Enum (Coord dims Mod), Enum (Coord dims Clamp))
  => Grid dims Double
  -> Grid dims Double
pacmanGauss = autoConvolute @'[3, 3] @Mod gauss'
  where gauss' g = sum g / fromIntegral (length g)
