import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let (x:xs:_) = init (C.split '\n' contents)
    let (_:n:_) = map readInt (C.split ' ' x)
    
    print . fst $ foldl (\(amt, tL) x -> let n = readInt x in if tL-n >= 0 then (amt+1, tL-n) else (amt, 0)) (0, n) (C.split ' ' xs)