import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, when)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    xs <- replicateM 9 getInt

    let xsSum = sum xs
    let (n1, n2) = findIncorrectNumbers xsSum xs 
    mapM_ (\x -> when (x /= n1 && x /= n2) $ print x) xs

    where
        findIncorrectNumbers xsSum (h:xs)
            | incorrectNum1 /= 0 = (h, incorrectNum1)
            | otherwise = findIncorrectNumbers xsSum xs
            where
                incorrectNum1 = foldl (\a x -> if xsSum-h-x == 100 then x else a) 0 xs