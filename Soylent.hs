import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    replicateM_ n handle

    where
        handle = do
            n <- getInt
            print ((n+399) `div` 400)