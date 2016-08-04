module DNA (toRNA) where

import Control.Applicative
import qualified Data.Map as Map (fromList, lookup)


single :: Char -> Maybe Char
single = flip Map.lookup $ (Map.fromList [('C', 'G'), ('T', 'A'), ('A', 'U'), ('G', 'C')])

-- | if string contains invalid character, return Nothing
-- | if string contains only valid nucleotides, return Just transcription
toRNA :: String -> Maybe String
toRNA s = foldr (\x y -> pure (:) <*> x <*> y)  (Just "" ) (map single s)
