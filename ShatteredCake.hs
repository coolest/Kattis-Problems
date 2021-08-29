import qualified Data.ByteString.Char8 as C
import Control.Monad
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    contents <- C.getContents
    
    let w:n:xs = C.split '\n' contents

    print $ sum (map (\x -> product (map readInt (C.split ' ' x))) xs) `div` (readInt w)