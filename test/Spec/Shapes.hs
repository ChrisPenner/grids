{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Shapes (spec) where

import Test.Hspec hiding (focus)
import qualified Data.Vector as V
import Data.Grid as G
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
  describe "partitionFocus" $ do
    it "should split properly" $ do
      let g :: Grid [2, 2] Int = autoConvolute @[3, 3] omitBounds (sum . Compose . fmap join . snd . partitionFocus . getCompose) smallGrid
      let sums = fromNestedLists' [[6, 5], [4, 3]]
      g `shouldBe` sums
