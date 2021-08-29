import Control.Monad (replicateM)

f = getLine
    >>= (\[a, b, c] -> putStrLn (show a ++ " " ++ show b ++ " " ++ show c))
        . (\[a, b] -> [60/(b/(a-1)), 60/(b/a), 60/(b/(a+1))]) 
        . map (\x -> read x :: Float) 
        . words

main = getLine 
    >>= (`replicateM` f) 
        . (\x -> read x :: Int)