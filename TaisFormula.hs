f _ 0 a = return a
f [n1, n2] n a = do
    [n1', n2'] <- map (\x -> read x :: Double) . words <$> getLine
    f [n1', n2'] (n-1) (a+(n2'+n2)/2 * (n1'-n1))

main = do
    cases <- (\x -> read x :: Int) <$> getLine
    l <- map (\x -> read x :: Double) . words <$> getLine
    ans <- f l (cases-1) 0
    print (ans/1000)