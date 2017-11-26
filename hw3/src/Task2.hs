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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

word :: Parser String
word = lexeme $ some letterChar

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
  try (word >>= \varname -> return (Var varname)) <|>
  (integer >>= \value -> return (Lit value))

parseProduct :: Parser Expr
parseProduct =
  try (parseBinaryExpr parseAtom parseExpression "*" Mul) <|>
  try (parseBinaryExpr parseAtom parseExpression "/" Div) <|>
  parseAtom

parseLetExpression :: Parser Expr
parseLetExpression = do
  _ <- symbol "let"
  varname <- word
  _ <- symbol "="
  what <- parseExpression
  _ <- symbol "in"
  inwhere <- parseExpression
  return $ Let varname what inwhere

parseExpression :: Parser Expr
parseExpression =
  try (parens parseLetExpression) <|>
  try (parens parseExpression) <|>
  try (parseBinaryExpr parseProduct parseExpression "+" Add) <|>
  try (parseBinaryExpr parseProduct parseExpression "-" Sub) <|>
  parseProduct

main :: String -> IO ()
main = parseTest parseExpression
