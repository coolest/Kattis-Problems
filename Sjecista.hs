import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt
    
    print (n * (n-1) * (n-2) * (n-3) `div` 24)