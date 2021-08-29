import Control.Monad(replicateM)
import Data.Char (digitToInt)

getLineAsInt = (\x -> read x :: Int) <$> getLine
check x n = sum (map digitToInt n) == x

main = do
    [min, max, x] <- replicateM 3 getLineAsInt

    putStrLn $ repeater x add1ToStr (show min)
    putStrLn $ repeater x sub1ToStr (show max)

    where
        repeater x f = until (check x) f
        add1ToStr x = show (1+(read x :: Int))
        sub1ToStr x = show ((read x :: Int)-1)