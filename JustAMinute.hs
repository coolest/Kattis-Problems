import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getDoubles = map (fromIntegral . readInt) . C.split ' ' <$> C.getLine

main = do
    n <- getInt
    xs <- replicateM n getDoubles

    let totalN = foldl (\a xs -> a + head xs) 0 xs
    let ans = foldl (\a [m, x] -> a+x/60) 0 xs / totalN
    if ans <= 1
        then putStrLn "measurement error"
        else print ans