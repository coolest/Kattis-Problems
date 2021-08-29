repeatUntilNZero :: Float -> Float -> IO ()
repeatUntilNZero 0 _ = return ()
repeatUntilNZero n max = do
    dimension <- getLine
    if (read dimension :: Float) > max 
        then putStrLn "NE"
        else putStrLn "DA"

    repeatUntilNZero (n-1) max

main = do
    line <- getLine
    let numbers = map (\x -> read x :: Float) (words line)
    let matches = head numbers
    let cutoffDimension = sqrt $ (numbers !! 1) ^ 2 + last numbers ^ 2
    repeatUntilNZero matches cutoffDimension
