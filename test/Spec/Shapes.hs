{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Shapes (spec) where

import Test.Hspec hiding (focus)
import qualified Data.Vector as V
import Data.Grid as G
import Data.Grid.Shapes as G
import Control.Applicative
import Data.Maybe
import Data.Functor.Compose
import Control.Comonad
import Control.Monad
import Data.Coerce

smallGrid :: Grid '[2, 2] Int
smallGrid = generate id

medGrid :: Grid '[3, 3] Int
medGrid = generate id

spec :: Spec
spec = 
  describe "neighbouringWindow" $ do
    it "should not change on round trip" $ do
      let g = convolute (neighbouringWindow @'[3, 3]) focus medGrid
      g `shouldBe` medGrid

    it "should select the proper neighbours" $ do
      let g :: Grid [2, 2] Int = convolute (omitBounds . neighbouringWindow @'[3, 3]) (sum . Compose .  neighbours . getCompose) smallGrid
      let sums = fromNestedLists' [[6, 5], [4, 3]]
      g `shouldBe` sums
