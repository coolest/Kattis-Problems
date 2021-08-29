import qualified Data.ByteString.Char8 as C
import Control.Monad (unless)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    [n, d] <- getInts

    unless (n == 0 && d == 0) $ do
        let wholeNum = n `div` d
        let r = n `mod` d
        printf "%d %d / %d\n" wholeNum r d
        main