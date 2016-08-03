module Bob (responseFor) where
import Data.Char

data Bob = Question | Yell | Address | Other

instance Show Bob where
  show Question = "Sure."
  show Yell = "Whoa, chill out!"
  show Address = "Fine. Be that way!"
  show Other = "Whatever."

isHell x = (any isAlpha x) && (x == (map toUpper x))
isQuestion x = length x > 0 && last x == '?'
isAddress x = all isSpace x

parse :: String -> Bob
parse s
  | isHell s = Yell
  | isQuestion s = Question
  | isAddress s = Address
  | otherwise = Other

responseFor :: String -> String
responseFor = show . parse
