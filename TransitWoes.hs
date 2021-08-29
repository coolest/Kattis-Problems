getInts = map (\x -> read x :: Int) . words <$> getLine
main = do
    [x0, e, bn] <- getInts
    walkTimes <- getInts
    busRideTimes <- getInts
    busArrivalIntervals <- getInts

    if fst (foldl (\(a, i) t -> (t-(a`mod`t)+(walkTimes !! i)+(busRideTimes !! i), i+1)) (0, 0) busArrivalIntervals) <= e 
        then putStrLn "yes"
        else putStrLn "no"
