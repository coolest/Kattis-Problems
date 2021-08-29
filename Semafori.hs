import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

rep time [(h, ha), (t, ta)] = if time - h < 0 then (time, ha == "r") else rep (time-h) [(t, ta), (h, ha)]

main = do
    contents <- C.getContents

    let (x:xs) = init (C.split '\n' contents)
    let (_:d:_) = map readInt (C.split ' ' x)

    handle 0 0 0 d xs

    where
        handle :: Int -> Int -> Int -> Int-> [C.ByteString] -> IO ()
        handle t pt cd d [] = print (t+d-cd);
        handle t pt cd d (x:xs) = if cond
            then handle (nT+(r-remT)) t' (cd+t'-pt) d xs 
            else handle nT t' (cd+t'-pt) d xs
            where
                (remT, cond) = rep nT [(r, "r"), (g, "g")]
                [t', r, g] = map readInt (C.split ' ' x)
                nT = t+t'-pt

