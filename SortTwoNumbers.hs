main = do
    [a, b] <- map (\x -> read x :: Int) . words <$> getLine
    if b > a
        then putStrLn (show a ++ " " ++ show b)
        else putStrLn (show b ++ " " ++ show a)