isVowel x 
    | x == 'a' || x == 'e' || x == 'o' || x == 'i' || x == 'u' = True
    | otherwise = False

main = do
    [y, p] <- words <$> getLine

    let l@[l1, l2] = [last (init y), last y]
    let y' = init y

    if l == "ex" 
        then putStrLn (y ++ p)
        else if isVowel l2
            then putStrLn (y' ++ "ex" ++ p)
            else if l2 == 'e' 
                then putStrLn (y ++ "x" ++ p)
                else putStrLn (y ++ "ex" ++ p)
