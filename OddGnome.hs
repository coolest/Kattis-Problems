import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, when, foldM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    cases <- getInt

    replicateM_ cases handle

    where
        handle = do
            (h:gnomes) <- tail <$> getInts
            foldM' 1 h gnomes

            where 
                foldM' i a (x:xs)
                    | a+1 /= x = print (i+1)
                    | otherwise = foldM' (i+1) x xs