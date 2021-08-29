import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst. fromJust . C.readInt

toOpposite = C.map (\c -> if c == '0' then '1' else '0') 

main = do
    contents <- C.getContents

    let (n:init:end:_) = C.words contents
    let isOdd = odd (readInt n)

    if (not isOdd && init == end) || (isOdd && toOpposite init == end)
        then putStrLn "Deletion succeeded"
        else putStrLn "Deletion failed"