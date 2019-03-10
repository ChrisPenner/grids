{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Shapes (spec) where

import Test.Hspec hiding (focus)
import Data.Grid as G
import Data.Functor.Compose
import Control.Monad

smallGrid :: Grid '[2, 2] Int
smallGrid = generate id

spec :: Spec
spec = 
  describe "partitionFocus" $ do
    it "should split properly" $ do
      let g :: Grid [2, 2] Int = autoConvolute @[3, 3] omitBounds (sum . Compose . fmap join . snd . partitionFocus . getCompose) smallGrid
      let sums = fromNestedLists' [[6, 5], [4, 3]]
      g `shouldBe` sums
