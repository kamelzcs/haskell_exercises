import Data.Char (digitToInt)
import Data.List (maximumBy)


digitSum = sum . map digitToInt . show

main = print $ maximum $ map digitSum nums
  where
    nums = [a^b | a <- [1..99], b <- [1..99]]
