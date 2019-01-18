module Data.Grid.Examples.Intro where

import Data.Grid

simpleGrid :: Grid L '[5, 5] Int
simpleGrid = generate id

modGrid :: Grid M '[5, 5] Int
modGrid = generate id

clampedGrid :: Grid C '[2, 2, 2, 2, 2, 2, 2] Int
clampedGrid = generate id

-- simpleMath :: 
