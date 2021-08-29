main = do
    iterations <- getLine >>= \x -> return (read x :: Int)
    let n = 0.5 + (2*(2^iterations)+1)/2
    print (round (n^2) :: Int)
