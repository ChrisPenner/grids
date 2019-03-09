{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec where

import qualified Spec.Grid as Grid
import qualified Spec.Shapes as Shapes
import Test.Hspec

main :: IO ()
main = hspec $ do
  Grid.spec
  Shapes.spec
