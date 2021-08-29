import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Control.Monad (mapM_)
import Data.Char (intToDigit, digitToInt)

readInt = fst . fromJust . C.readInt

toNum 'a' = 1;toNum 'b' = 2;toNum 'c' = 3;toNum 'd' = 4;toNum 'e' = 5;toNum 'f' = 6;toNum 'g' = 7;toNum 'h' = 8; toNum _ = 0
toLet 1 = 'a';toLet 2 = 'b';toLet 3 = 'c';toLet 4 = 'd';toLet 5 = 'e';toLet 6 = 'f';toLet 7 = 'g';toLet 8 = 'h'; toLet _ = 'N'

moves = [(2, 1), (-2, 1), (2, -1), (-2, -1), (1, 2), (-1, 2), (1, -2), (-1, -2)]

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    let ans = map solve xs
    
    where
        solve :: C.ByteString -> [String]
        solve x = foldl (\acc (a, b) -> let (nA, nB) = (toNum l + a, n + b) in if nA > 0 && nA < 9 && nB > 0 && nB < 9 then [toLet nA, intToDigit nB]:acc else acc) [] moves
            where
                l = C.head x
                n = digitToInt (C.index x 1)
