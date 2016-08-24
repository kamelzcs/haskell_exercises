module Raindrops (convert) where

import Control.Monad.Writer.Lazy(execWriterT,tell,listen)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

convert :: Int -> String
convert x = fromMaybe (show x) $ execWriterT answer
    where check n s = if mod x n == 0 then s else ""
          answer = do (_,res) <- listen $ tell (mconcat $ zipWith check [3,5,7] ["Pling","Plang","Plong"])
                      guard (res /= "")
