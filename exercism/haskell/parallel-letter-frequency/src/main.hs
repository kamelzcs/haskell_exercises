{-# LANGUAGE OverloadedStrings #-}
--
-- main.hs
-- Copyright (C) 2016 zhao <zhao@f45c89c59701.ant.amazon.com>
--
-- Distributed under terms of the MIT license.
--
--

import Frequency
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map



star_spangled_banner :: Text
star_spangled_banner = T.concat
  ["O say can you see by the dawn's early light,"
  ,"What so proudly we hailed at the twilight's last gleaming,"
  ,"Whose broad stripes and bright stars through the perilous fight,"
  ,"O'er the ramparts we watched, were so gallantly streaming?"
  ,"And the rockets' red glare, the bombs bursting in air,"
  ,"Gave proof through the night that our flag was still there;"
  ,"O say does that star-spangled banner yet wave,"
  ,"O'er the land of the free and the home of the brave?"
  ]


main = print $ Map.lookup 'a' $ frequency 2 [star_spangled_banner]
