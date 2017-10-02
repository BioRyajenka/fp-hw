module Test5 where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Semigroup ((<>))
import           Data.Monoid (Sum (..), mempty) 

import           Task3
import           Task5

spec :: IO ()
spec = hspec $ do
    it "maybeConcat" $ maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]
    it "eitherConcat" $ eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])

    it "NonEmpty" $ do
        let a = (1 :: Int) :| [2]
        let b = (2 :: Int) :| [3]
        let c = (3 :: Int) :| [4]
        (a <> b) <> c `shouldBe` a <> (b <> c)
    it "Identity" $ do
        let a = (1 :: Int) :| [2]
        a <> mempty `shouldBe` a
        mempty <> a `shouldBe` a
    it "Tree" $ do
        let a = fromList [1, 2, 3]
        a <> mempty `shouldBe` a
        mempty <> a `shouldBe` a