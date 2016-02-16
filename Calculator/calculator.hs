module Parser where
import qualified Prelude
import Prelude hiding ((++))
import Data.Char

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Monad Parser where
    return a = Parser $ \cs -> [(a, cs)]
    p >>= f = Parser $ \cs -> concat [parse (f a) cs' |(a, cs') <- parse p cs]

item :: Parser Char
item = Parser $ \cs -> case cs of
                           "" -> []
                           (c: cs') -> [(c, cs')]

class Monad m => MonadZero m where
        zero :: m a

class MonadZero m => MonadPlus m where
        (++) :: m a -> m a -> m a

instance MonadZero Parser where
        zero = Parser $ const []

instance MonadPlus Parser where
        p ++ q = Parser $ \cs -> parse p cs Prelude.++ parse q cs

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case parse (p ++ q) cs of
                              [] -> []
                              (x : _) -> [x]


sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- item
        if p c then return c else zero

char :: Char -> Parser Char
char c = sat (c == )

string :: String -> Parser String
string "" = return ""
string (c : cs) = do
        char c
        string cs
        return (c : cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
        a <- p
        as <- many p
        return $ a : as

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
        a <- p
        as <- many (do {sep; p})
        return $ a : as

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
        a <- p
        rest a
        where rest a = (do f <- op
                           b <- p
                           rest (f a b))
                       +++ return a

space :: Parser String
space = many $ sat isSpace

token :: Parser a -> Parser a
token p = do
        a <- p
        space
        return a

symb :: String -> Parser String
symb cs = token $ string cs

apply :: Parser a -> String -> [(a, String)]
apply p = parse $ do
                  space
                  p

expr :: Parser Int
expr = term `chainl1` addop

addop :: Parser(Int -> Int -> Int)
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

term :: Parser Int
term = factor `chainl1` mulop

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return div}

factor :: Parser Int
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Int
digit = do
        x <- token $ sat isDigit
        return $ ord x - ord '0'
