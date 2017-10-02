module Main where

import Text.Read
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

removeAt :: Int -> [a] -> (Either a String, [a])
removeAt n x 
    | length x < n = (Left (x!!n), take n x ++ drop (n + 1) x)
    | otherwise    = (Right "There is no such element", x)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k list = (filter' (/=) zipped, filter' (==) zipped) where
    zipped = zip (concat $ repeat [1..k]) list 
    filter' :: (Int -> Int -> Bool) -> [(Int, a)] -> [a]
    filter' f list' = map snd $ filter (\x -> f (fst x) k) list'

stringSum :: String -> Maybe Integer
stringSum string = foldl (\acc m -> (+) <$> acc <*> m) (Just 0) nums
    where
        nums = map (readMaybe . removePlus) $ words string
        removePlus s = case s of 
            ('+':xs) -> xs
            _ -> s

mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list = merge (mergeSort $ leftHalf list) (mergeSort $ rightHalf list) where
    merge (x:xs) (y:ys)
        | x > y     = y:merge (x:xs) ys
        | otherwise = x:merge xs (y:ys)
    merge xs ys  = xs ++ ys
    leftHalf xs  = take (length xs `div` 2) xs
    rightHalf xs = drop (length xs `div` 2) xs

main :: IO ()
main = return ()