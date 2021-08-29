import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))
    handle xs

    where
        solve n 0 = 0
        solve n x = if n == ans then x else solve n (x-1)
            where
                ans = foldl (\a c -> a+digitToInt c) 0 (show x)

        handle [] = return ()
        handle (x:xs) = do
            let n = readInt x
            let ans = C.foldl (\a c -> a+digitToInt c) 0 x

            print (solve (ans-1) (n-1))

            handle xs

