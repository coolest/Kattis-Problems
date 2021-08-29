import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    r <- fromIntegral <$> getInt

    print (pi * r**2)
    print (r*r*2)
    