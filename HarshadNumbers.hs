import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)

strip (Just a) = a

getInt = fst . strip . C.readInt <$> C.getLine

main = do
    n <- getInt
    
    print $ until f (+1) n

    where
        f n = n `rem` (sum . map digitToInt $ show n) == 0
