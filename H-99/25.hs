import System.Random
import Control.Monad.Trans.Class


deleteAt :: Int -> [a] -> [a]
deleteAt p xs = let (h, t) = splitAt p xs
                in h ++ tail t

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu [x] = return [x]
rnd_permu xs = do p <- getStdRandom $ randomR (0, (length xs)-1)
                  let left = deleteAt p xs
                  left' <- rnd_permu left
                  return $ (xs !! p) : left'

main = rnd_permu "abcdef" >>= print
