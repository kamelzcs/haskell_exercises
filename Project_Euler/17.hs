import Data.Char
import Debug.Trace

one = ["one","two","three","four","five","six","seven","eight",
     "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
     "sixteen","seventeen","eighteen", "nineteen"]
ty = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

decompose x
    | x == 1000                    = "onethousand"
    | x >= 100 && x `mod` 100 /= 0          = 
        one !! (firstDigit (x)-1) ++ "hundred" ++ "and" ++decompose ( x - firstDigit (x) * 100)
    | x >= 100 = 
        one !! (firstDigit (x)-1) ++ "hundred"
    | x >= 20           = 
        ty !! (firstDigit (x) - 2) ++ decompose ( x - firstDigit (x) * 10)
    | x >0                       = one !! (x-1)
    | x == 0                       = []
    where firstDigit = digitToInt . head . show

solve = length . concatMap decompose $ [1..1000]

main = print solve
