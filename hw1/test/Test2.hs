module Test2(spec) where

import           Control.Exception (evaluate)
import           Data.List         (sort)
import           Test.Hspec
import           Test.QuickCheck

import           TestLib           (listToIO, randomIntList)

import           Task2

spec :: IO ()
spec = hspec $ do
    it "removeAt" $ do
        removeAt 1 [1, 2, 3] `shouldBe` (Just 2, [1, 3])
        removeAt 10 [1..9] `shouldBe` (Nothing, [1..9])
        removeAt 1 "abcd" `shouldBe` (Just 'b', "acd")
    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8], [3,6])
        collectEvery 1 [1..5] `shouldBe` ([], [1..5])
    it "stringSum" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97

        let mustPass = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
                       , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
                       , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
                       , "+1", "1 +1", "-1 +1", "+1 -1" ]
        let mustFail = ["asd", "1-1", "1.2", "--2", "1+", "1+1", "++1", "-+1", "+-1", "1 + 1"]

        listToIO $ map (\x -> evaluate (stringSum x) `shouldThrow` anyException) mustFail
        listToIO $ map (\x -> stringSum x `shouldSatisfy` const True) mustPass
    it "mergeSort" $ do
        --let test = (=<<) $ randomIntList 100 1 100
        test1 <- randomIntList 5 1 10
        mergeSort test1 `shouldBe` sort test1

        test2 <- randomIntList 100 1 100
        mergeSort test2 `shouldBe` sort test2

        test3 <- randomIntList 100 (-100) 100
        mergeSort test3 `shouldBe` sort test3

        test4 <- randomIntList 100 (-100) 100
        mergeSort test4 `shouldBe` sort test4

        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
