import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    [n, p1, p2] <- getInts
    let totalPoints = p1+p2

    if odd (totalPoints `div` n)
        then putStrLn "opponent"
        else putStrLn "paul"