module WordProblem (answer) where

-- Fantastic reference: http://dev.stephendiehl.com/fun/002_parsers.htm

import Control.Applicative
import Text.Parsec.String (Parser)
import Text.Parsec (runParser, ParseError)
import Text.Parsec.Char (anyChar, string, spaces)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Char
import Text.Parsec.Combinator (many1, chainl1)
import Data.Either

-- number = [ "-" ] digit { digit }.
-- digit  = "0" | "1" | ... | "8" | "9".
-- expr   = number { op number }.
-- op  = "plus" | "minus" | "multiplied" | "divided" | "cubed".

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Cube Expr Expr
  | Lit Int
  deriving Show

whatis :: Parser ()
whatis = (string "What is") >> spaces

number :: Parser Expr
number = do {a <- int; spaces; return $ Lit a}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

expr :: Parser Expr
expr = number `chainl1` op

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

op :: Parser (Expr -> Expr -> Expr)
op = (infixOp "plus" Add) <|> (infixOp "divided by" Div) <|> (string "m" >> ((infixOp "inus" Sub) <|> (infixOp "ultiplied by" Mul)))

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Div a b -> eval a `div` eval b
  Lit n   -> n

run :: String -> Either ParseError Expr
run = runParser (whatis >> expr <* (string "?")) () "test"

answer :: String -> Maybe Int
answer s =  $ fmap eval (run s)
