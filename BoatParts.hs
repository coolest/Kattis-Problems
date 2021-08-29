import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    [p, d] <- getInts
    r <- replicateM d C.getLine

    putStrLn $ f ([], 0) p r

    where
        f (a, d) p l
            | length a == p = show d
            | null l = "paradox avoided"
            | head l `notElem` a = f (head l : a, d+1) p (tail l)
            | otherwise = f (a, d+1) p (tail l)
