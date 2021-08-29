import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInt = fst . strip . C.readInt <$> C.getLine
getNegInts = filter (< 0) . map (fst . strip . C.readInt) . C.words <$> C.getLine

main = do
    getLine
    expenses <- getNegInts
    print . (*(-1)) . sum $ expenses
    