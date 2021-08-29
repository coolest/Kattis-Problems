import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, replicateM, unless)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    n <- getInt

    handle 1 n

    where
        handle c m = do
            [n, n'] <- getInts
            matrix <- map C.reverse . reverse <$> replicateM n C.getLine

            putStrLn ("Test " ++ show c)
            mapM_ C.putStrLn matrix

            unless (c == m) $ do
                handle (c+1) m

