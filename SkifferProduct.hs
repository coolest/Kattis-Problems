import Data.Char (digitToInt)

main = do
    line <- getLine

    handle line

    where
        handle x
            | prodOfDigits < 10 = print prodOfDigits
            | otherwise = handle (show prodOfDigits)
            where 
                prodOfDigits = foldl (\a x -> a*digitToInt x) 1 fX
                fX = filter (/='0') x