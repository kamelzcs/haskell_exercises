data ListItem a = Single a | Multiple Int a
    deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap toStr
    where toStr (Single x) = [x]
          toStr (Multiple i x) = replicate i x

main = print $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
