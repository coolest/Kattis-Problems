import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let (a:b:c:n:_) = map readInt (C.split ' ' $ head (C.split '\n' contents))
    if a > 0 && b > 0 && c > 0 && a+b+c >= n && n >= 3
        then putStrLn "YES"
        else putStrLn "NO"
