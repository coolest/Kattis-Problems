toInt x = read x :: Int

main = do
    [x, y] <- map toInt . words <$> getLine
    if even y
        then putStrLn "possible"
        else putStrLn "impossible"