import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInt = strip . C.readInt <$> C.getLine

main = do
    (n, _) <- getInt
    l <- replicateM n (length <$> getLine)
    mapM print l