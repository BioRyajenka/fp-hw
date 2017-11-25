module Task1
       ( Expr(..),
         ArithmeticError(..),
         exec
       ) where

import           Control.Monad.Reader
import qualified Data.Map             as Map

data Expr =
    Var String
  | Expr `Add` Expr
  | Expr `Mul` Expr
  | Expr `Div` Expr
  | Let String Expr Expr

data ArithmeticError = ArithmeticError

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

exec :: Expr -> Reader (Map.Map String Integer) (Either ArithmeticError Integer)
exec expr = reader $ \varsMap -> execWithMap varsMap expr where
    execWithMap :: Map.Map String Integer -> Expr -> Either ArithmeticError Integer
    execWithMap varsMap (Var varname) = maybeToEither ArithmeticError $ Map.lookup varname varsMap
    execWithMap varsMap (a `Add` b) = fmap (+) (execWithMap varsMap a) <*> execWithMap varsMap b
    execWithMap varsMap (a `Mul` b) = fmap (*) (execWithMap varsMap a) <*> execWithMap varsMap b
    execWithMap varsMap (a `Div` b) = fmap quot (execWithMap varsMap a) <*> execWithMap varsMap b
    execWithMap varsMap (Let varname what inwhere) =
      if Map.member varname varsMap
      then Left ArithmeticError
      else execWithMap varsMap what >>= \newwhat -> execWithMap (Map.insert varname newwhat varsMap) inwhere
