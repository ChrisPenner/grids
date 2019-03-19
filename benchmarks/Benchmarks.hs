{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Benchmarks where

import Gauge.Main
import Data.Grid as G
import Data.Functor.Compose

main :: IO ()
main =
    defaultMain [ bgroup "convolution"
                         [ bgroup "boxBlur"
                                  [
                                  -- bench "nf [3, 3] window over [100, 100]"
                                  --       $ nf (benchBlur @[3, 3])
                                  --            (doubleGrid @[100, 100])
                                   bench "nf [10, 10] window over [100, 100]"
                                        $ nf (benchBlur @[10, 10])
                                             (doubleGrid @[100, 100])
                                  -- , bench "nf [3, 3] window over [200, 200]"
                                  --       $ nf (benchBlur @[3, 3])
                                  --            (doubleGrid @[200, 200])
                                  ]
                         , bgroup "sobel"
                                  [ bench "nf [200, 200]"
                                        $ nf benchSobel
                                             (doubleGrid @[200, 200])
                                  ]
                         ]
                ]

doubleGrid :: IsGrid dims => Grid dims Double
doubleGrid = fromIntegral <$> generate id

benchBlur :: forall window dims x y.
           (IsGrid dims, IsGrid window, dims ~ [x, y])
           => Grid dims Double
           -> Grid dims Double
benchBlur = boxBlur @window


boxBlur :: forall window dims. (IsGrid dims, IsGrid window) => Grid dims Double -> Grid dims Double
boxBlur = autoConvolute omitBounds gauss'
  where
    gauss' :: Compose (Grid window) Maybe Double -> Double
    gauss' g = (sum g) / fromIntegral (length g)

benchSobel :: IsGrid dims => Grid dims Double -> Grid dims Double
benchSobel = autoConvolute clampBounds sobel

sobel :: Floating a => Grid [3, 3] a -> a
sobel g = sqrt $ (v ** 2) + (h ** 2)
    where
        v = sum (g * sobelVertical)
        h = sum (g * sobelHorizontal)

sobelHorizontal :: Floating a => Grid [3, 3] a
sobelHorizontal = fromNestedLists'
    [ [-1, -2, -1]
    , [0, 0, 0]
    , [1, 2, 1]
    ]

sobelVertical :: Floating a => Grid [3, 3] a
sobelVertical = fromNestedLists'
    [ [-1, 0, 1]
    , [-2, 0, 2]
    , [-1, 0, 1]
    ]
