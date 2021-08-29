import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    [a1, a2, c] <- getInts


    print $ f (a1+a2) c 0
    
    where 
        f a c t = if (a-c) >= 0
            then f (a-c+1) c (t+1)
            else t
