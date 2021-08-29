import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    n <- getInt
    xs <- getInts

    ans <- handle xs 0 0
    print ans

    where
        handle [] pn towers = return towers
        handle (x:xs) pn towers
            | x > pn = handle xs x (towers+1)
            | otherwise = handle xs x towers