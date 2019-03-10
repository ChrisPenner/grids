{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Benchmarks where

import Gauge.Main
import Data.Grid as G
import Data.Functor.Compose

benchGauss :: forall window dims x y.
           (IsGrid dims, IsGrid window, dims ~ [x, y])
           => Grid dims Double
           -> Grid dims Double
benchGauss = gauss @window

idGrid :: IsGrid dims => Grid dims Int
idGrid = generate id

gauss :: forall window dims. (IsGrid dims, IsGrid window) => Grid dims Double -> Grid dims Double
gauss = autoConvolute omitBounds gauss'
  where
    gauss' :: Compose (Grid window) Maybe Double -> Double
    gauss' g = (sum g) / fromIntegral (length g)


main :: IO ()
main =
  defaultMain [ bgroup "convolution"
                       [ bgroup "gauss"
                                [ bench "nf [3, 3] window over [100, 100]"
                                    $ nf (benchGauss @[3, 3]) (fromIntegral <$> idGrid @[100, 100])
                                , bench "nf [10, 10] window over [100, 100]"
                                    $ nf (benchGauss @[10, 10]) (fromIntegral <$> idGrid @[100, 100])
                                , bench "nf [3, 3] window over [500, 500]"
                                    $ nf (benchGauss @[3, 3]) (fromIntegral <$> idGrid @[500, 500])
                                ]
                       ]
              ]
