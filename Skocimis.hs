handle :: [Int] -> Int
handle input = if middle - left > right - middle
    then middle - left - 1
    else right - middle - 1
    where
        left = head input
        middle = input !! 1
        right = last input

main = do
    input <- getLine
    
    print $ handle $ map read (words input)

