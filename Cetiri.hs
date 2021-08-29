import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.List (sort)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let [a, b, c] = sort . map readInt 
            . C.words . head
            $ C.lines contents

    let n = minimum . map abs $ [b-a, c-a, a-b, a-c, c-b, b-c]

    if a+n*2 == b
        then print (a+n)
        else if b+n*2 == c
            then print (b+n)
            else print (c+n)