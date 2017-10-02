module Task2
       ( removeAt
       , collectEvery
       , stringSum
       , mergeSort
       ) where

--import           Text.Read     (readEither)

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n x
    | n < length x = (Just (x!!n), take n x ++ drop (n + 1) x)
    | otherwise    = (Nothing, x)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k list = (filter' (/=) zipped, filter' (==) zipped) where
    zipped = zip (concat $ repeat [1..k]) list
    filter' :: (Int -> Int -> Bool) -> [(Int, a)] -> [a]
    filter' f list' = map snd $ filter (\x -> f (fst x) k) list'

stringSum :: String -> Integer
stringSum string = sum nums
    where
        nums = map (read . removePlus) $ words string
        removePlus s = case s of
            ('+':'-':_) -> error "Can't parse"
            ('+':xs)    -> xs
            _           -> s

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
