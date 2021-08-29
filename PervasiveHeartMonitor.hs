import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (C.split '\n' contents)

    handle xs

    where
        handle :: [C.ByteString] -> IO ()
        handle [] = return ()
        handle (x:xs) = do
            let name = C.unwords (reverse names)
            let avg = sum beats / (fromIntegral . length $ beats)

            printf "%.2f %s\n" avg (C.unpack name)
            handle xs

            where
                (names, beats) = foldl (\(n, b) bstr -> if isDigit (C.head bstr) then (n, read (C.unpack bstr) : b) else (bstr : n, b)) ([], []) (C.split ' ' x) :: ([C.ByteString], [Double])
