module Task4
       ( splitOn
       , joinWith
       ) where

import           Data.Maybe (fromMaybe)

import           Task3      (Tree (..))

instance Foldable Tree where
   foldr _ z Leaf               = z
   foldr f z (Node v Leaf Leaf) = f v z
   foldr f z (Node v l r)       = foldr f (f v (foldr f z r)) l

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]] where
    f c l@(x:xs)
        | c == delimiter = []:l
        | otherwise      = (c:x):xs
    f _ _ = error "this code shouldn't be executed"

joinWith :: Char -> [String] -> String
joinWith c s = fromMaybe "" $ foldl f Nothing s where
    f Nothing xs    = Just xs
    f (Just acc) xs = Just (acc ++ [c] ++ xs)
