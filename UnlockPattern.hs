import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)
import Data.List (elemIndex)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

fold_ :: Int -> [[Int]] -> Int -> (Double, Double)
fold_ ind (x:xs) n = if n `elem` x
    then (fromIntegral ind, fromIntegral $ strip (n `elemIndex` x)) 
    else fold_ (ind+1) xs n

main = do
    code <- replicateM 3 getInts
    
    print (handle code 0 1)

    where
        handle :: [[Int]] -> Double -> Int -> Double
        handle code distance 9 = distance
        handle code distance cN = handle code (distance + sqrt ((x'-x)**2 + (y'-y)**2)) (cN+1)
            where
                (x, y) = fold_ 0 code cN
                (x', y') = fold_ 0 code (cN+1)