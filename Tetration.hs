
main = do
    n <- (\x -> read x :: Double) <$> getLine

    print (n ** (1/n))