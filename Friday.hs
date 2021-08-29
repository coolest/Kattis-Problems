import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

calc d _ 0 = (False, d)
calc d cD n = if d == 5 && cD == 13 then (True, (d+n)`rem`7) else calc (if d == 7 then 1 else d+1) (cD+1) (n-1)

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle xs

    where
        handle [] = return ()
        handle (_:x:xs) = do
            let days = map readInt (C.split ' ' x)
            let (_, ans) = foldl (\(d, ans) ds -> let (f, newD) = calc d 1 ds in if f then (newD, ans+1) else (newD, ans)) (7, 0) days
            
            print ans
            
            handle xs