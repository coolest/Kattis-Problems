import qualified Data.ByteString.Char8 as C
import Data.List 

strip (Just a) = a

getInts = sort . map (fst . strip . C.readInt) . C.words <$> C.getLine

main = do
    ints@[a, b, c, d] <- getInts
    let s = fromIntegral (sum ints) / (2 :: Double)
    print . sqrt . product $ map ((s-) . fromIntegral) ints

