module Test1 where

--import           Control.Exception (evaluate)
import           Test.Hspec
--import           Test.QuickCheck

import           Task2

spec :: IO ()
spec = hspec $ do
  it "_1" $ do
      view _1 ("a", 2::Int) `shouldBe` "a"
      set _1 (3::Int) (1::Int, 3::Int) `shouldBe` (3::Int, 3::Int)
      over _1 succ (1::Int, 3::Int) `shouldBe` (2::Int, 3::Int)
  it "_2" $ do
      view _2 ("a", 2::Int) `shouldBe` 2
      set _2 (4::Int) (1::Int, 3::Int) `shouldBe` (1::Int, 4::Int)
      over _2 succ (1::Int, 3::Int) `shouldBe` (1::Int, 4::Int)
