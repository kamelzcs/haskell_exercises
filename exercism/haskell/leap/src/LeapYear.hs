module LeapYear (isLeapYear) where

isLeapYear n
  | (n `rem` 400 == 0) || ((n `rem` 4 == 0) && (n `rem` 100 /= 0)) = True
  | otherwise = False
