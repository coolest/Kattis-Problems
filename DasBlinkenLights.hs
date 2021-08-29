import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    [p, q, s] <- getInts

    if p < q
        then simulate (p, p) (q, q) s
        else simulate (q, q) (p, p) s

    where
        simulate (inc1, ans1) (inc2, ans2) max
            | ans1 > max = putStrLn "no"
            | ans1 == ans2 = putStrLn "yes"
            | (ans1+inc1) <= ans2 = simulate (inc1, ans1+inc1) (inc2, ans2) max
            | otherwise = simulate (inc1, ans1+inc1) (inc2, ans2+inc2) max
