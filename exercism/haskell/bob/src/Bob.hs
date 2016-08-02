module Bob (responseFor) where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec


data Bob = Question | Yell | Address | Other

instance Show Bob where
  show Question = "Sure."
  show Yell = "Whoa, chill out!"
  show Address = "Fine. Be that way!"
  show Other = "Whatever."

parseQuesion :: Parser Bob
parseQuesion = endWith '?' >> return Question

endWith :: Char -> Parser ()
endWith x = many anyChar >> char x >> return ()

parseYell :: Parser Bob
parseYell = (endWith '!' <|> allCapital) >> return Yell
  where
    allCapital = many (upper <|> space) >> char '?' >> return ()

parseAddress :: Parser Bob
parseAddress = (string "\n\r \t\v\xA0\x2002" <|> many space) >> return Address

parseOther :: Parser Bob
parseOther = many anyChar >> return Other

parseBob :: Parser Bob
parseBob = parseYell <|> parseQuesion <|> parseAddress <|> parseOther

responseFor :: String -> String
responseFor input = case parse parseBob "Bob" input of
  Left err -> "No match: " ++ show err
  Right val -> show val
