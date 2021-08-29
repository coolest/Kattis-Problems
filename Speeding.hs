import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

thrd (_, _, a) = a

main = do
    n <- getInt
    ([il, id]:speeds) <- replicateM n getInts

    print 
        . maximum 
        . map thrd 
        $ scanl (\(l, d, _) [l', d'] -> (l', d', (d'-d) `div` (l'-l))) (il, id, 0) speeds