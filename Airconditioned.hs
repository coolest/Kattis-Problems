qs [] = []
qs ((a, b):xs) = qs (filter (\(a', b') -> a' < a || (a' == a && b' - a' <= b - a)) xs)
    ++ [(a, b)]
    ++ qs (filter (\(a', b') -> a' > a || (a' == a && b' - a' > b - a)) xs)

getMinions l 0 = return l
getMinions l n = do
    [min, max] <- words <$> getLine >>= \x -> return (map (\y -> read y :: Int) x)
    getMinions ((min, max):l) (n-1)

getAns _ [] r = r
getAns (a, b) ((a', b'):l) r = if b >= a'
    then getAns (a, b) l r
    else getAns (a', b') l (r+1)

main = do 
    n <- (\x -> read x :: Int) 
        <$> getLine
    h:minions <- qs 
        <$> (getMinions [] n)

    print $ getAns h minions 1


-- i need to work on my problem solving skills because this was extremely difficult to me despite the fact that this was relatively simple.
