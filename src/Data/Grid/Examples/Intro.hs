module Data.Grid.Examples.Intro where

import Data.Grid

simpleGrid :: Grid Unsafe '[5, 5] Int
simpleGrid = generate id
