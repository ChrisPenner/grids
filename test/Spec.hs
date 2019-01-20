{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Spec where

import Test.Hspec
import qualified Data.Vector as V
import Data.Grid as G
import Control.Applicative
import Data.Maybe
import Data.Functor.Compose


smallGrid :: Grid '[2, 2] Int
smallGrid = G.generate id

medGrid :: Grid '[3, 3] Int
medGrid = G.generate id

dim3Grid :: Grid '[2, 3, 4] Int
dim3Grid = G.generate id




main :: IO ()
main = hspec $ do
  describe "creation" $ do
    it "pure should make a grid of all one element" $ do
      let Grid v = pure 1 :: Grid '[2, 2] Int
      v `shouldBe` V.replicate 4 1

    it "generate should put things in the right places" $ do
      let g        = G.generate id :: Grid '[2, 2] Int
          expected = fromNestedLists [[0, 1], [2, 3]]
      Just g `shouldBe` expected

    it "tabulate should put coords in the right places" $ do
      let g        = tabulate id :: Grid '[2, 2] (Coord '[2, 2] Clamp)
          expected = fromNestedLists
            [[Coord [0, 0], Coord [0, 1]], [Coord [1, 0], Coord [1, 1]]]
      Just g `shouldBe` expected

  describe "indexing" $ do
    it "index should retrieve correct elem" $ do
      (smallGrid `index` Coord [0, 0]) `shouldBe` 0
      (smallGrid `index` Coord [0, 1]) `shouldBe` 1
      (smallGrid `index` Coord [1, 0]) `shouldBe` 2
      (smallGrid `index` Coord [1, 1]) `shouldBe` 3

  describe "applicative" $ do
    it "should apply piecewise" $ do
      let expected = fromJust . fromNestedLists $ [[0, 2], [4, 6]]
      liftA2 (+) smallGrid smallGrid `shouldBe` expected

  describe "nested lists" $ do
    it "toNestedLists" $ toNestedLists smallGrid `shouldBe` [[0, 1], [2, 3]]
    it "fromNestedLists"
      $          fromNestedLists [[0, 1], [2, 3]]
      `shouldBe` Just smallGrid
    it "fromList" $ do
      G.fromList [0, 1, 2, 3] `shouldBe` Just smallGrid

  describe "updates" $ do
    it "(//)"
      $ (    smallGrid
        G.// [(Coord [1, 1] :: Coord '[2, 2] Clamp, 42), (Coord [0, 1], 100)]
        )
      `shouldBe` fromNestedLists' [[0, 100], [2, 42]]

  describe "permutations" $ do
    it "transpose" $ do
      transpose smallGrid `shouldBe` fromNestedLists' [[0, 2], [1, 3]]
      transpose medGrid
        `shouldBe` fromNestedLists' [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
    it "permute" $ do
      permute @'[1, 2, 0] dim3Grid `shouldBe` fromNestedLists'
        [ [[0, 12], [1, 13], [2, 14], [3, 15]]
        , [[4, 16], [5, 17], [6, 18], [7, 19]]
        , [[8, 20], [9, 21], [10, 22], [11, 23]]
        ]

  describe "convolutions" $ do
    it "autoConvolute with Clamp clamps out of bounds" $ do
      autoConvolute @'[3, 3] @Clamp toNestedLists smallGrid
        `shouldBe` fromNestedLists'
                     [ [ [[0, 0, 1], [0, 0, 1], [2, 2, 3]]
                       , [[0, 1, 1], [0, 1, 1], [2, 3, 3]]
                       ]
                     , [ [[0, 0, 1], [2, 2, 3], [2, 2, 3]]
                       , [[0, 1, 1], [2, 3, 3], [2, 3, 3]]
                       ]
                     ]

    it "autoConvolute with Mod wraps when out of bounds" $ do
      autoConvolute @'[3, 3] @Mod toNestedLists smallGrid
        `shouldBe` fromNestedLists'
                     [ [ [[3, 2, 3], [1, 0, 1], [3, 2, 3]]
                       , [[2, 3, 2], [0, 1, 0], [2, 3, 2]]
                       ]
                     , [ [[1, 0, 1], [3, 2, 3], [1, 0, 1]]
                       , [[0, 1, 0], [2, 3, 2], [0, 1, 0]]
                       ]
                     ]

    it "safeAutoConvolute gets 'Nothing' for out of bounds" $ do
      safeAutoConvolute @'[3, 3] toNestedLists smallGrid
        `shouldBe` fromNestedLists'
                     [ [ [ [Nothing, Nothing, Nothing]
                         , [Nothing, Just 0, Just 1]
                         , [Nothing, Just 2, Just 3]
                         ]
                       , [ [Nothing, Nothing, Nothing]
                         , [Just 0, Just 1, Nothing]
                         , [Just 2, Just 3, Nothing]
                         ]
                       ]
                     , [ [ [Nothing, Just 0, Just 1]
                         , [Nothing, Just 2, Just 3]
                         , [Nothing, Nothing, Nothing]
                         ]
                       , [ [Just 0, Just 1, Nothing]
                         , [Just 2, Just 3, Nothing]
                         , [Nothing, Nothing, Nothing]
                         ]
                       ]
                     ]
