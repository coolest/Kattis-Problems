import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    [r, cR] <- map fromIntegral <$> getInts

    let tA = pi * r^2
    let cA = pi * cR^2
    let cheeseA = pi * (r-cR)^2

    let percentageOfCheese = cheeseA / tA
    print (percentageOfCheese*100)