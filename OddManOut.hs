import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, when, mapM_)
import Data.List (group, sort)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    n <- getInt

    handle n 1

    where
        handle n c = do
            if n == (c-1)
                then return ()
                else do
                    C.getLine
                    guests <- group . sort <$> getInts
                    mapM_ (\x -> when (length x == 1) $ putStrLn $ "Case #" ++ show c ++ ": " ++ (show . head $ x)) guests
                    handle n (c+1)
