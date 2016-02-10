import Control.Monad
import Control.Monad.State
import Data.Set

type Select element = StateT (Set element) [] element

select :: (Ord element) => [element] -> Select element
select as = do
    s <- get
    a <- lift as
    guard $ not $ member a s
    put $ insert a s
    return a

runSelect :: Select element -> [element]
runSelect m = Prelude.map fst $ runStateT m empty

fromDigits = Prelude.foldl (\number x -> 10 * number + x) 0

threeDigitsToNum a b c = 100 * a + 10 * b + c

candidates = runSelect $ do
  d4 <- select [0, 2..8]
  d3 <- select [0..9]
  d5 <- select [0..9]
  guard ((threeDigitsToNum d3 d4 d5) `mod` 3 == 0)
  d6 <- select [0,5]
  d7 <- select [0..9]
  guard ((threeDigitsToNum d5 d6 d7) `mod` 7 == 0)
  d8 <- select [0..9]
  guard ((threeDigitsToNum d6 d7 d8) `mod` 11 == 0)
  d9 <- select [0..9]
  guard ((threeDigitsToNum d7 d8 d9) `mod` 13 == 0)
  d10 <- select [0..9]
  guard ((threeDigitsToNum d8 d9 d10) `mod` 17 == 0)
  d2 <- select [0..9]
  d1 <- select [0..9]
  return (fromDigits [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10])

main = print $ sum candidates
