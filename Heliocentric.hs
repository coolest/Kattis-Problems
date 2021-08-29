import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (C.split '\n' contents)
    handle 1 xs

    where
        solve :: Int -> Int -> Int -> Int
        solve d e m
            | m == 0 && e == 0 = d
            | otherwise = solve (d+1) ((e+1)`rem`365) ((m+1)`rem`687)

        handle :: Int -> [C.ByteString] -> IO ()
        handle _ [] = return ()
        handle n (x:xs) = do
            let (e:m:_) = map readInt (C.split ' ' x)

            printf "Case %d: %d\n" n (solve 0 e m)

            handle (n+1) xs