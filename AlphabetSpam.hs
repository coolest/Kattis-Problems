import Data.Char

main = do
    line <- map ord <$> getLine
    
    let total = fromIntegral . length $ line
    let (ws, lc, uc, sym) = foldl (f) (0, 0, 0, 0) line :: (Double, Double, Double, Double)

    print $ ws / total
    print $ lc / total
    print $ uc / total
    print $ sym / total

    where 
        f (ws, lc, uc, sym) x
            | x == 95 = (ws+1, lc, uc, sym)
            | x >= 65 && x <= 90 = (ws, lc, uc+1, sym)
            | x >= 97 && x <= 122 = (ws, lc+1, uc, sym)
            | otherwise = (ws, lc, uc, sym+1)
