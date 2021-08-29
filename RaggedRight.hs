import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = C.lines contents
    let m = maximum 
            $ map C.length xs
    
    let ans = foldl (\acc x -> acc+(C.length x - m)^2) 0 . init $ xs
    
    print ans
    