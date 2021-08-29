import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, replicateM)
import Data.Char (isSpace)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    [xr, xc, r, c] <- getInts

    m <- unlines . magR r <$> replicateM xr getLine

    mapM (\x -> if isSpace x then putChar x else replicateM_ c (putChar x)) m

    where
        magR r m = concatMap (replicate r) m