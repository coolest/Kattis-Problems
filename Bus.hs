import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, mapM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    ans <- replicateM n (handle 0 <$> getInt)
    mapM print ans

    where
        handle n 0 = n
        handle n stops = handle (n*2 + 1) (stops-1)
