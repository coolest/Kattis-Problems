toInt x = read x :: Int

f :: Int -> String -> Int 
f a b = if '-' `elem` b 
    then 
        let (x1, x2) = span (/='-') b 
        in a + (1 - toInt x1 - toInt x2)
    else a+1

split :: Char -> String -> [String] -> [String]
split x xs r = 
    if x `elem` xs 
        then split x (tail xs') (p : r)
        else p : r
    where (p, xs') = span (/=x) xs

main = do
    line <- getLine
    let probs = split ';' line []

    print $ foldl f 0 probs