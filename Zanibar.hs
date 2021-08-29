import Control.Monad (replicateM)

getInt x = read x :: Int
clamp n1 x
    | n1 > x = n1
    | otherwise = x

c = clamp 0 

main = do
    n <- getInt <$> getLine
    replicateM n ( do
            p:ps <- map getInt . words <$> getLine
            print . fst $ foldl (\(a1, a2) b -> (a1+c(b - a2*2), b)) (0, p) ps
        )