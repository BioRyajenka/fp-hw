module Task2
       ( parseExpression
       ) where

import Task1(Expr(..))

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
--import System.Environment

import qualified Text.Megaparsec.Lexer as L

spaces :: Parser Char
spaces = char ' ' <|> char '\t'

sc :: Parser ()
sc = L.space (void spaces) (void spaces) (void spaces)

symbol :: String -> Parser String
symbol = L.symbol sc

{-
  Expression = (LetExpr) | (Expression) | Product + Expression | Product - Expression | Product
  LetExpr    = let Variable = Expression in Expression
  Product    = Atom * Product | Atom / Product | Atom
  Atom       = Variable | Literal
-}

parseBinaryExpr :: Parser Expr -> Parser Expr -> String -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinaryExpr parser1 parser2 op constructor = do
  a <- parser1
  _ <- symbol op
  b <- parser2
  return $ constructor a b

parseAtom :: Parser Expr
parseAtom =
  some letterChar >>= \varname -> return (Var varname) <|>
  (some digitChar >>= \value -> return (Lit (read value)))

parseProduct :: Parser Expr
parseProduct =
  try (parseBinaryExpr parseAtom parseProduct "*" Mul) <|>
  try (parseBinaryExpr parseAtom parseProduct "/" Div) <|>
  parseAtom

parseLetExpression :: Parser Expr
parseLetExpression = do
  _ <- symbol "let"
  varname <- some letterChar
  _ <- symbol "="
  what <- parseExpression
  _ <- symbol "in"
  inwhere <- parseExpression
  return $ Let varname what inwhere

parseExpression :: Parser Expr
parseExpression =
  try (between (symbol "(") (symbol ")") parseLetExpression) <|>
  between (symbol "(") (symbol ")") parseExpression <|>
  try (parseBinaryExpr parseProduct parseExpression "+" Add) <|>
  try (parseBinaryExpr parseProduct parseExpression "-" Sub) <|>
  parseProduct

--kek :: Parser String
--kek = do
  --(char '(' >>= (\ _ -> char 'a'))
  --between (symbol "(") (symbol ")") (char '$')
  --symbol "($" >>= (\ x -> symbol ")")

--kek = char '(' >>= (\ _ -> char 'a')

main :: String -> IO ()
main = parseTest parseExpression
