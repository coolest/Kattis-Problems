import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

inc 8 = 1
inc x = x + 1

main = do
    begin <- getInt
    n <- getInt

    ans <- handle begin 210
    print ans

    where
        handle :: Int -> Int -> IO Int
        handle p timeLeft = do
            [n, t] <- C.split ' ' <$> C.getLine
            if timeLeft-readInt n < 0 
                then return p
                else if t == C.pack "T"
                        then handle (inc p) (timeLeft - readInt n)
                        else handle p (timeLeft - readInt n)

