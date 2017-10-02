module Main where

-- import Lib
import Data.List
import Control.Monad

order3 :: (Ord a) => (a, a, a) -> [a]
order3 (a, b, c) = sort [a, b, c]

highestBit :: Int -> (Int, Int)
highestBit treshold' = gen treshold' 1 1 where
    gen treshold prev depth
        | prev * 2 > treshold = (prev, depth)
        | otherwise           = gen treshold (prev * 2) (depth + 1)

{-
highestBit :: Int -> (Int, Int)
highestBit treshold = maximum $ map mapfunction [1..] where
    mapfunction i 
        | 2^i <= treshold = (2 ^ i, i)
        | otherwise       = (0, 0)
-}

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap $ join replicate

contains :: Eq a => a -> [[a]] -> [[a]]
contains val = filter $ elem val

main :: IO ()
main = return ()