import Control.Monad
import Data.List

combines ::  Int -> [Int] -> [([Int], [Int])]
combines 0 xs = [([], xs)]
combines n xs = [(y : ys, rest) | y <- xs, (ys, rest) <- combines (n - 1) (delete y xs)]

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

valid s1 s2 = (sort $ show $ s1) == (show $ fromDigits s2)

pandigiticals = do
        (begin, end) <- combines 5 [1..9]
        l <- [1, 2]
        let (first, second) = splitAt l begin
            res = fromDigits first * fromDigits second
        guard $ valid res end
        return $ res

solve = sum $ nub $ pandigiticals

main = print $ solve
