{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Grid.Examples.Conway where

import Data.Grid
import Data.Foldable
import Data.List
import Data.Functor.Compose

rule' :: Grid [3, 3] Bool -> Bool
rule' (partitionFocus -> (currentCellAlive,  neighbours)) = (currentCellAlive && livingNeighbours == 2) || livingNeighbours == 3
  where
    livingNeighbours = length . filter id . toList . Compose $ neighbours

step
  :: (Dimensions dims)
  => Grid dims Bool -> Grid dims Bool
step = autoConvolute @[3, 3] wrapBounds rule'

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
