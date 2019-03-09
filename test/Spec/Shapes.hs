{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Shapes (spec) where

import Test.Hspec
import qualified Data.Vector as V
import Data.Grid as G
import Data.Grid.Shapes as G
import Control.Applicative
import Data.Maybe
import Data.Functor.Compose
import Control.Comonad

medGrid :: Grid '[3, 3] Int
medGrid = generate id

spec :: Spec
spec = do
  describe "neighbouringWindow" $ do
    it "should not change on round trip" $ do
      let g = convolute (neighbouringWindow @'[3, 3]) extract medGrid
      g `shouldBe` medGrid
