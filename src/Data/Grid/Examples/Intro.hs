module Data.Grid.Examples.Intro where

import Data.Grid

simpleGrid :: Grid Unsafe '[5, 5] Int
simpleGrid = generate id

modGrid :: Grid Modular '[5, 5] Int
modGrid = generate id

clampedGrid :: Grid Clamped '[5, 5] Int
clampedGrid = generate id
