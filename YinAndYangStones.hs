import qualified Data.ByteString.Char8 as C

main = do
    line <- C.getLine

    if length (C.elemIndices 'W' line) == length (C.elemIndices 'B' line)
        then putStrLn "1"
        else putStrLn "0"