#! /usr/bin/env runhugs +l
--
-- 1.hs
-- Copyright (C) 2015 zhao <zhao@kamel-ThinkPad-X201>
--
-- Distributed under terms of the MIT license.
--


import Data.List (union)
problem_1' = sum (union [3,6..999] [5,10..999])

main :: IO ()
main = do
    print problem_1'
