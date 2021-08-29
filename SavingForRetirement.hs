import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    [b, bR, bS, a, aS] <- getInts

    let bobMoney = bS * (bR - b)

    print ((bobMoney+aS) `div` aS + a)