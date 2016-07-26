import Data.Monoid (mappend)
import Data.List (sortBy, sort, group, nub, elemIndex, partition)
import Control.Arrow ((&&&), (***))
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad (join)

type Hand = [(Int, Char)]
type Values = [Int]
data Ranking = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind  | StraightFlush deriving (Eq, Ord)
data HandValue = HandValue Ranking Values deriving (Eq, Ord)

sortDecreasing :: Ord a => [a] -> [a]
sortDecreasing = sortBy (flip compare)

rateHand :: Hand -> HandValue
rateHand hand
   | straight && flush = HandValue StraightFlush sortedRanks
   | flush = HandValue Flush sortedRanks
   | straight = HandValue Straight sortedRanks
   | otherwise = case map fst groups of
                         [4, 1] -> HandValue FourOfAKind values
                         [3, 2] -> HandValue FullHouse values
                         [3, 1, 1] -> HandValue ThreeOfAKind values
                         [2, 2, 1] -> HandValue TwoPair values
                         [2, 1, 1, 1] -> HandValue Pair values
                         otherwise -> HandValue HighCard values
                      where
       sortedRanks = sortDecreasing $ map fst hand
       suits = map snd hand
       flush = length (nub suits) == 1
       straight = sortedRanks == reverse [last sortedRanks..head sortedRanks] || sortedRanks == [12,3,2,1,0]
       groups = sortDecreasing $ map (length &&& id) $ group sortedRanks
       values = concatMap snd groups

mapOverPair :: (a -> b) -> (a, a) -> (b, b)
mapOverPair f (x, y) = (f x, f y)

parseHand :: String -> Hand
parseHand str = zip ranks suits
  where (suits, ranksChars) = partition isSuit str
        isSuit = (`elem` "SCDH")
        ranks = map (fromJust . (`elemIndex` "23456789TJQKA")) ranksChars

parseLine :: String -> (Hand, Hand)
parseLine = mapOverPair parseHand . splitAt 10 . filter (/= ' ')

playerOneWins :: (Hand, Hand) -> Bool
playerOneWins (h1, h2) = rateHand h1 > rateHand h2

main :: IO ()
main = do
  hands <- (map parseLine . lines) <$> readFile "p054_poker.txt"
  print $ length $ filter playerOneWins hands
