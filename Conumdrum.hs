main = do
    text <- getLine

    print . snd $ foldl (\(i, a) x -> if repl !! (i `rem` 3) == x then (i+1, a) else (i+1, a+1)) (0, 0) text

    where
        repl = "PER"