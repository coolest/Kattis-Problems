import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = map readInt 
            $ C.split ' ' (last $ init (C.split '\n' contents))

    handle xs (40, 40) (1, 1)

    where
        handle :: [Int] -> (Int, Int) -> (Int, Int) -> IO ()
        handle [_, _] (a1, a2) (n, n') = printf "%d %d" n (maximum [a1, a2])
        handle (a:b:c:xs) (a1, a2) (n, n') = if maximum [a, c, a1, a2] /= maximum [a, c] then handle (b:c:xs) (a, c) (n', n'+1) else handle (b:c:xs) (a1, a2) (n, n'+1)


