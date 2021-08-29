import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, unless)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    handle 1 n

    where
        handle n m = do
            line <- last . C.words <$> C.getLine
            let a = readInt $ C.takeWhile (/='/') line
            let b = readInt . C.tail $ C.dropWhile (/='/') line
            --printf "a:%d b:%d" a b
            printf "%d %d\n" n (solve a b)

            unless (n == m) $ do
                handle (n+1) m

            where
                solve :: Int -> Int -> Int
                solve a b
                    | a > b = 1 + 2*solve(a-b) b
                    | a < b = 2*solve a (b-a)
                    | otherwise = 1