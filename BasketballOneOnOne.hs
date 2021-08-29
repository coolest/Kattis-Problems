import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit, digitToInt)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    line <- C.getLine
    putStrLn $ handle line 0 0
    
    where
        handle :: C.ByteString -> Int -> Int -> String
        handle bstr a b
            | a >= 11 && a - b >= 2 = "A"
            | b >= 11 && b - a >= 2 = "B"
            | team == 'A' = handle bstr' (a+digitToInt pts) b
            | team == 'B' = handle bstr' a (b+digitToInt pts)
            where
                team = C.head bstr
                pts = C.head (C.tail bstr)
                bstr' = C.drop 2 bstr