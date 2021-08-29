import qualified Data.ByteString.Char8 as C
import Data.List (sort)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    n <- getInt
    
    let bs = [1..n]
    gs <- sort <$> getInts

    let ans = handle gs bs 100
    if ans >= 0
        then print ans
        else putStrLn "impossible"

    where
        handle :: [Int] -> [Int] -> Float -> Float
        handle [] [] ans = ans
        handle (g:gs) (b:bs) ans
            | g > b = -1
            | otherwise = handle gs bs (if ans > a then a else ans)
            where
                a = fromIntegral g / fromIntegral b


