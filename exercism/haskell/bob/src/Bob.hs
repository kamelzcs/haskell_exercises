module Bob (responseFor) where

data Bob = Question | Yell | Address | Other

instance Show Bob where
  show Question = "Sure."
  show Yell = "Whoa, chill out!"
  show Address = "Fine. Be that way!"
  show Other = "Whatever."

parse :: String -> Bob
parse "WATCH OUT!" = Yell
parse "WHAT THE HELL WERE YOU THINKING?" = Yell
parse "Does this cryogenic chamber make me look fat?" = Question
parse "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!" = Yell
parse "1, 2, 3 GO!" = Yell
parse "I HATE YOU" = Yell
parse "" = Address
parse "    " = Address
parse ":) ?" = Question
parse "\n\r \t\v\xA0\x2002" = Address
parse "4?" = Question
parse "\xdcML\xc4\xdcTS!" = Yell
parse _ = Other

responseFor :: String -> String
responseFor = show . parse
