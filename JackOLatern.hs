main = do
    ans <- getLine >>= \x -> return . product $ map (\y -> read y :: Int) (words x)
    print ans