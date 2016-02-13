isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = let h = head xs
                      t = last xs
                  in (h == t) && (isPalindrome $ tail $ init xs)
