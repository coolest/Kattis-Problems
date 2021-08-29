import Control.Monad (replicateM)

f = getLine 
        >>= (\[c, d] -> putStrLn (show c ++ " " ++ show (d*(d+1)`div`2+d))) 
            . map (\x -> read x :: Int) 
            . words
main = getLine 
        >>= \x -> replicateM (read x :: Int) f