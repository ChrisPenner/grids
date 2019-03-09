{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Grid.Examples.Conway where

import Data.Grid
import Data.Grid.Shapes
import Data.Foldable
import Data.List
import Data.Functor.Compose
import Data.Functor.Rep
import Control.Arrow
import Data.Monoid

rule' :: Neighbours window Bool -> Bool
rule' (Neighbours (currentCellAlive,  neighbours)) = (currentCellAlive && livingNeighbours == 2) || livingNeighbours == 3
  where
    livingNeighbours = length . filter id . toList $ neighbours

step
  :: (Dimensions dims)
  => Grid dims Bool -> Grid dims Bool
step = convolute (wrapBounds . neighbouringWindow @'[3, 3]) rule'

glider :: [Coord '[10, 10]]
glider = Coord <$> [[0, 1], [1, 2], [2, 0], [2, 1], [2, 2]]

start :: Grid '[10, 10] Bool
start = tabulate (`elem` glider)

simulate :: Int -> Grid '[10, 10] Bool
simulate = (iterate step start !!)

showBool :: Bool -> Char
showBool True = '#'
showBool False = '.'

showGrid
  :: (Dimensions '[x, y])
  => Grid '[x, y] Bool -> String
showGrid = intercalate "\n" . toNestedLists . fmap showBool
