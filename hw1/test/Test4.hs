module Test4 where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Task4

spec :: IO ()
spec = hspec $ do
    it "splitOn" $ do
        splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
        splitOn '/' "/path/" `shouldBe` ["", "path", ""]
    it "joinWith" $ do
        joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
        joinWith '/' ["", "path", ""] `shouldBe` "/path/"
