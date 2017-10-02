module TestLib
       ( randomIntList
       , listToIO
       ) where

import Test.Hspec

import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

listToIO :: [Expectation] -> IO ()
listToIO = foldl (>>) (return ())