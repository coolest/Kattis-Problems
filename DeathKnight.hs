import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

combo = C.pack "CD"

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    let ans = foldl (\acc bstr -> if not (combo `C.isInfixOf` bstr) then acc+1 else acc) 0 xs

    print ans