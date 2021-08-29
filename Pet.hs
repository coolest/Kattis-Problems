import Control.Monad (replicateM)

max' [] iteration i v = (show i, show v)
max' (x:xs) iteration i v
    | x > v = max' xs (iteration+1) iteration x
    | otherwise = max' xs (iteration+1) i v

main = do
    grades <- replicateM 5 (do
        getLine >>= \x -> return . sum $ map (\y -> read y :: Int) (words x))
    
    let (line, grade) = max' grades 1 0 0
    putStrLn $ line ++ " " ++ grade
