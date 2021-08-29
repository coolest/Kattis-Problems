import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (C.split '\n' contents)

    handle 1 xs

    where
        handle :: Int -> [C.ByteString] -> IO ()
        handle _ [] = return ()
        handle n (x:xs)
            | note == C.pack "A#" = do printf "Case %d: " n; putStr "Bb "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "Bb" = do printf "Case %d: " n; putStr "A# "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "C#" = do printf "Case %d: " n; putStr "Db "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "Db" = do printf "Case %d: " n; putStr "C# "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "D#" = do printf "Case %d: " n; putStr "Eb "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "Eb" = do printf "Case %d: " n; putStr "D# "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "F#" = do printf "Case %d: " n; putStr "Gb "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "Gb" = do printf "Case %d: " n; putStr "F# "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "G#" = do printf "Case %d: " n; putStr "Ab "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | note == C.pack "Ab" = do printf "Case %d: " n; putStr "G# "; C.putStr tonalty; putChar '\n'; handle (n+1) xs
            | otherwise = printf "Case %d: UNIQUE" n
            where
                (note:tonalty:_) = C.split ' ' x

