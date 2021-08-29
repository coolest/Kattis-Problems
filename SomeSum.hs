main = do
    n <- read <$> getLine

    case (even (sum [1..n]), even (sum [0..(n-1)])) of
        (True, True) -> putStrLn "Even"
        (False, False) -> putStrLn "Odd"
        _ -> putStrLn "Either"