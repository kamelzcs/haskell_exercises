module Parser where
import qualified Prelude
import Prelude hiding ((++))
import Data.Char
import Data.List
import Control.Applicative hiding (many) -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap


instance Monad Parser where
    return a = Parser $ \cs -> [(a, cs)]
    p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs]
    p >> q = Parser $ \cs -> concat [parse q cs' | (_, cs') <- parse p cs]

type Binary = String

data Exp = C Float | Op Binary Exp Exp

instance Show Exp where
                show (C x) = show x
                show (Op op l r) = "(" ++ show l ++ op ++ show r ++ ")"

ops :: Fractional a => [(Binary, a->a->a)]
ops = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]

item :: Parser Char
item = Parser $ \cs -> case cs of
                           "" -> []
                           (c: cs') -> [(c, cs')]

class Monad m => MonadZero m where
        zero :: m a

class MonadZero m => MonadPlus m where
        mplus :: m a -> m a -> m a

instance MonadZero Parser where
        zero = Parser $ const []

instance MonadPlus Parser where
        p `mplus` q = Parser $ \cs -> parse p cs ++ parse q cs

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case parse (p `mplus` q) cs of
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

apply :: Parser Exp -> String -> [(Maybe Float, String)]
apply exp = parse $ space >> (fmap eval exp)

eval :: Exp -> Maybe Float
eval (C x) = Just x
eval (Op "/" _ y) | Just 0 <- eval y = Nothing
eval (Op op l r) = do
                op <- lookup op ops
                lv <- eval l
                rv <- eval r
                return $ op lv rv

expr :: Parser Exp
expr = term `chainl1` addop

addop :: Parser (Exp -> Exp -> Exp)
addop = (symb "+" >> return (Op "+")) +++ (symb "-" >> return (Op "-"))

term :: Parser Exp
term = factor `chainl1` mulop

mulop :: Parser (Exp -> Exp -> Exp)
mulop = (symb "*" >> return (Op "*")) +++ (symb "/" >> return (Op "/"))

factor :: Parser Exp
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Exp
digit = do
        x <- token $ many1 $ sat isDigit
        return $ C $ read x
