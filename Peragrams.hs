import Data.List (group)

sort :: String -> String
sort [] = []
sort (c:str) = sort less ++ [c] ++ sort more
    where
        less = filter (<c) str
        more = filter (>=c) str

countPairlessChars :: [[Char]] -> Int
countPairlessChars = foldr handle 0
    where 
        handle list acc = 
            if even $ length list
                then acc
                else acc + 1

clamp :: Int -> Int -> Int -> Int
clamp min max num 
    | num > max = max
    | num < min = min
    | otherwise = num

main = do
    letters <- getLine
    print 
        $ clamp 0 1000
        $ subtract 1
        $ countPairlessChars
        $ group
        $ sort letters
