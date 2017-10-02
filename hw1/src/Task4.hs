module Task4 where

import Task3

instance Foldable Tree where
   foldr _ z Leaf = z
   foldr f z (Node v Leaf Leaf) = f v z
   foldr f z (Node v l r) = foldr f (f v (foldr f z r)) l

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]] where
    f c l@(x:xs) 
        | c == delimiter = []:l
        | otherwise      = (c:x):xs
    f _ _ = error "this code shouldn't be executed"

joinWith :: Char -> [String] -> String
joinWith c = foldl f "" where
    f "" xs  = xs
    f acc xs = acc ++ [c] ++ xs