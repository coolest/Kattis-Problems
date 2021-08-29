import qualified Data.ByteString.Char8 as C
import Control.Monad 
import Data.List

strip (Just a) = a

getInt = strip . C.readInt <$> C.getLine

rmdups = map head . group . sort

main = do
    ans <- map (\(n, _) -> n`rem`42) <$> replicateM 10 getInt

    print . length $ rmdups ans