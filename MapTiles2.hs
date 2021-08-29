import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getLine

    let (x, y, z) = handle n (0, 0) 0
    printf "%d %d %d" z x y

    where
        handle :: String -> (Int, Int) -> Int -> (Int, Int, Int)
        handle [] (x, y) z = (x, y, z)
        handle (c:cs) (x, y) z
            | c == '0' = handle cs (x*2, y*2) (z+1)
            | c == '1' = handle cs (x*2+1, y*2) (z+1)
            | c == '2' = handle cs (x*2, y*2+1) (z+1)
            | c == '3' = handle cs (x*2+1, y*2+1) (z+1)

