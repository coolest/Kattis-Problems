main = getLine
    >>= putStrLn . concatMap (\x -> if x =='e' then "ee" else [x])