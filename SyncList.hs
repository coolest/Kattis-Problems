import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, when)
import Data.List (sortOn, sort)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

snd' (_, x, _) = x

main = do
    n <- getInt

    when (n/=0) (do putChar '\n'; f n; main)

    where 
        f n = do
            l <- (\(h:l) -> scanl (\x' x -> (fst x' + 1, x)) (0, h) l) 
                <$> replicateM n (getInt)
            l2 <- sort <$> replicateM n (getInt)

            let p@(h':l') = sortOn snd l
            let ans = sortOn snd' $ foldl (\a@((i', _, _):xs) (i, _) -> (i'+1, i,  l2 !! (i'+1)) : a) [(0, fst h', l2 !! 0)] l'

            mapM_ (\(_, _, x) -> print x) ans
    