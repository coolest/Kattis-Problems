import Data.List (groupBy, sortBy)
import Text.Printf

getData d = do
    newD@(x:xs) <- words <$> getLine
    if x == "-1"
        then return d
        else getData (newD : d)

main = do
    d <- groupBy (\[a, b, _] [a', b', _] -> b == b') 
        . sortBy (\[a, b, _] [a', b', _] -> b `compare` b') 
        <$> getData []
        
    let (time, correct) = handle d (0, 0)
    printf "%d %d\n" correct time

    where
        handle :: [[[String]]] -> (Int, Int) -> (Int, Int)
        handle [] (a, c) = (a, c)
        handle (g:d) (a, c) = if hasCorrectAns g 
            then handle d (a+20*(length g - 1)+timeTaken g, c+1)
            else handle d (a, c)
            where
                timeTaken ([a, b, c]:xs)
                    | c == "right" = read a
                    | otherwise = timeTaken xs
                hasCorrectAns [] = False
                hasCorrectAns (x:xs)
                    | last x == "right" = True
                    | otherwise = hasCorrectAns xs