import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst. fromJust . C.readInt

main = do
    contents <- C.getContents

    let tblr = C.transpose (tail (C.words contents))
    let unbroke@(t:b:l:[r]) = map (C.length . C.filter (/='1')) tblr :: [Int]
    let amt = minimum [t+b, l+r] `div` 2

    if amt == 0
        then printf "0 %d %d" (t+b) (l+r)
        else printf "%d %d %d" amt ((t+b) - amt*2) ((r+l) - amt*2)