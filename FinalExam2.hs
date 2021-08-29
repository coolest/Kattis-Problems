import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt
    (x:xs) <- replicateM n (do x <- getChar; getChar; return x)

    print . fst $ foldl (\(a, x') x -> if x == x' then (a+1, x) else (a, x)) (0, x) xs
