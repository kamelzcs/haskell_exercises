import Control.Arrow (***)

data Value = Two | Three | Four | Five| Six| Seven| Eight| Nine| Ten| Jack| Queen| King| Ace
data Kind = H | C | S| D
type Values = [Value]
data Ranking = HighCard | Pair| TwoPairs| ThreeOfAKind| Straight| Flush| FullHouse| FourOfAKind| StraightFlush| RoyalFlush
data Hand = Ranking Values

mapOverPair :: (a -> b) -> (a, a) -> (b, b)
mapOverPair f (x, y) = (f x, f y)

stringToKindValue = (parseKind &&& parseValue) . reverse

kindValuesToHand kvs = 

parseHand :: [String] -> Hand
parseHand = kindValuesToHand . map stringToKindValue

parseLine :: String -> (Hand, Hand)
parseLine = mapOverPair parseHand . splitAt 5 . words

main :: IO ()
main =
  hands <- (map parseLine . lines) <$> readFile "p054.txt"
  print $ length $ filter playerOneWins hands
