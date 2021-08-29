import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    gunnar <- sum <$> getInts
    emma <- sum <$> getInts

    case gunnar `compare` emma of
        LT -> putStrLn "Emma"
        GT -> putStrLn "Gunnar"
        EQ -> putStrLn "Tie"
