main = do
    ans <- getLine >>= \x -> return . sum $ map (\y -> read y :: Int) (words x)
    print ans