import Data.List (elemIndices)

main = do
    sAmt0 <- elemIndices 'S' <$> getLine
    sAmt1 <- elemIndices 'S' <$> getLine

    let amt = length sAmt0*length sAmt1
    if amt == 0
        then putStrLn "0"
        else do
            let firstHalf = format amt []
            putStrLn $ concat (firstHalf ++ replicate amt ")")

    where
        format 0 ans = reverse ("0":ans)
        format amt ans = format (amt-1) ("S(":ans)


