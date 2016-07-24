import Control.Arrow ((&&&))
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Function

data Value = Two | Three | Four | Five| Six| Seven| Eight| Nine| Ten| Jack| Queen| King| Ace deriving (Eq, Ord, Enum, Show)
data Kind = H | C | S| D deriving (Eq, Ord, Show)
type Values = [Value]
data Ranking = HighCard | Pair| TwoPairs| ThreeOfAKind| Straight| Flush| FullHouse| FourOfAKind| StraightFlush deriving (Eq, Ord, Show)
data Hand = Hand Ranking Values deriving (Eq, Ord, Show)

mapOverPair :: (a -> b) -> (a, a) -> (b, b)
mapOverPair f (x, y) = (f x, f y)

parseKind :: String -> Kind
parseKind = fromJust . flip lookup table . head
  where table = [('H', H), ('C', C), ('S', S), ('D', D)]

parseValue :: String -> Value
parseValue s = fromJust $ flip lookup table $ s !! 1
  where table = [('2', Two), ('3', Three), ('4', Four), ('5', Five), ('6', Six), ('7', Seven), ('8', Eight), ('9', Nine), ('T', Ten), ('J', Jack), ('Q', Queen), ('K', King), ('A', Ace)]

playerOneWins :: (Hand, Hand) -> Bool
playerOneWins (h1, h2) = h1 > h2

stringToKindValue :: String -> (Kind, Value)
stringToKindValue = (parseKind &&& parseValue) . reverse

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

kindValuesToHand :: [(Kind, Value)] -> Hand
kindValuesToHand kvs
  | straightFlush = Hand StraightFlush values
  | fourofAKind = Hand FourOfAKind values
  | fullHouse = Hand FullHouse values
  | flush = Hand Flush values
  | straight = Hand Straight values
  | threeOfAKind = Hand ThreeOfAKind values
  | twoPairs = Hand TwoPairs values
  | pair = Hand Pair values
  | otherwise = Hand HighCard values
  where
    sortedValues = sortDesc $ map snd kvs
    sortedKinds = sortDesc $ map fst kvs
    groupValues = sortDesc $ map (length &&& id) $ group sortedValues
    values = concatMap snd groupValues
    groupCounts = map fst groupValues
    straight = ((== 4) $ (-) (fromEnum $ head values) (fromEnum $ last values)) && (groupCounts == replicate 5 1)
    flush = (==) (head sortedKinds) (last sortedKinds)
    straightFlush = straight && flush
    fourofAKind = groupCounts == [4, 1]
    fullHouse = groupCounts == [3, 2]
    threeOfAKind = groupCounts == [3, 1, 1]
    twoPairs = groupCounts == [2, 2, 1]
    pair = groupCounts == [2, 1, 1, 1]

parseHand :: [String] -> Hand
parseHand = kindValuesToHand . sort . map stringToKindValue

parseLine :: String -> (Hand, Hand)
parseLine = mapOverPair parseHand . splitAt 5 . words

main :: IO ()
main = do
  hands <- (map parseLine . lines) <$> readFile "p054_poker.txt"
  print $ length $ filter playerOneWins hands
