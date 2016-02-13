pack [] = [[]]
pack [x] = [[x]]
pack (x : ys@(y : _)) = let ps = pack ys
                            as = if (x /= y) then ([x] : ps) else ((x : head ps) : (tail ps))
                        in as
