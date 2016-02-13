pack [] = [[]]
pack [x] = [[x]]
pack (x : ys@(y : _)) = let ps = pack ys
                            as = if (x /= y) then ([x] : ps) else ((x : head ps) : (tail ps))
                        in as

encode :: Eq a => [a] -> [(Int, a)]
encode = Prelude.map (\x -> (length x, head x)) . pack
