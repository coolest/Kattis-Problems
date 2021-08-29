import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let ([m, _]:xs) = map (map readInt . C.words) 
            $ C.lines contents

    let (p, ans) = foldl (handle m) (0, True) xs

    if p == 0 && ans
        then putStrLn "possible"
        else putStrLn "impossible"

    where
        handle m (c, b) [l, e, w] = if l > c || c-l+e < 0 || c-l+e > m || (w > 0 && c+e-l /= m) || not b
            then (c, False)
            else (c+e-l, True)
    