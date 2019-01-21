module Benchmarks where

import Gauge.Main
import Data.Grid as G

benchTranspose :: forall dims a x y. (Dimensions dims) =>  (dims ~ [x, y]) => Int -> (Grid dims Int, Grid dims Int, Grid dims Int)
benchTranspose a = (transpose $ pure 1, transpose $ pure 2, transpose $ pure 3)

main :: IO ()
main = defaultMain
  [ bgroup
      "permutations"
      [ bench "nf [150, 150]" $ nf (benchTranspose @[200, 200]) 2
      ]
  ]
