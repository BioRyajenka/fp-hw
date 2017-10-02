module Test1 where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Task1

spec :: IO ()
spec = hspec $ do
    it "order3" $ do
        order3 (-1, 2, -2) `shouldBe` (-2, -1, 2)
        order3 (5, 2, 10) `shouldBe` (2, 5, 10)
        order3 (True, True, False) `shouldBe` (False, True, True)
    it "highestBit" $ do
        highestBit 0 `shouldBe` (1, 1)
        highestBit 1 `shouldBe` (1, 1)
        highestBit 2 `shouldBe` (2, 2)
        highestBit 1000 `shouldBe` (512, 10)
    it "smartReplicate" $ do
        smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
        smartReplicate [0, 1] `shouldBe` [1]
    it "contains" $ do
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]
        contains 12345 [[1..5], [2,0], [3,4]] `shouldBe` []
