import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInt = fst . strip . C.readInt <$> C.getLine
getInts = map (fst . strip . C.readInt) . C.words <$> C.getLine

main = do
    n <- getInt
    replicateM n f

    where
        f = do
            [a, b, c] <- getInts
            if (a * b == c) || ((a `div` b == c && a `mod` b == 0) || (b `div` a == c && b `mod` a == 0)) || (a + b == c) || (a - b == c || b - a == c) 
                then putStrLn "Possible"
                else putStrLn "Impossible"