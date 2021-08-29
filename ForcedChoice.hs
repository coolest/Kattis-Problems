import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    [_, s, n] <- getInts
    replicateM n (f s)

    where
        f s = do
            (_:cards) <- getInts
            if s `elem` cards
                then putStrLn "KEEP"
                else putStrLn "REMOVE"