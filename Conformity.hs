import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.List (group, sort, maximumBy)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    let ordXS = group . sort 
            $ map (sort . C.words) xs

    let m = length 
            $ maximumBy (\a b -> length a `compare` length b) ordXS
    let all = filter (\a -> m == length a) ordXS

    if m == 1
        then print (length ordXS)
        else print (m * length all)