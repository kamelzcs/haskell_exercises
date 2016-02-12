elementAt as x = snd $ last $ takeWhile ((>= 1) . fst) (zip [x, x - 1..1] as)
