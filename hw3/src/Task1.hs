module Task1
       ( Expr

       ) where

import qualified Data.Map as Map

data Expr = Lit Int
  | Var String
  | Expr `Add` Expr
  | Expr `Mul` Expr
  | Expr `Div` Expr
  | Let String Expr Expr

exec :: Expr -> Maybe Int
exec = execWithMap Map.empty where
    {- [(String, Maybe Int)] -}
    execWithMap :: Map.Map String Int -> Expr -> Maybe Int
    execWithMap varsMap (Var varname) = Map.lookup varname varsMap
    execWithMap varsMap (a `Add` b) = fmap (+) (execWithMap varsMap a) <*> (execWithMap varsMap b)
    execWithMap varsMap (a `Mul` b) = fmap (*) (execWithMap varsMap a) <*> (execWithMap varsMap b)
    execWithMap varsMap (a `Div` b) = fmap quot (execWithMap varsMap a) <*> (execWithMap varsMap b)
