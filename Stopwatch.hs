repeatTask :: Int -> IO Int -> IO Int -> IO Int
repeatTask 0 _ solution = solution
repeatTask loops action solution = 
    do
        n1 <- action
        n2 <- solution
        repeatTask (loops-1) action (return $ n2 + n1)

getAliveTime :: IO Int
getAliveTime =
    do
        startTime <- getLine
        endTime <- getLine
        return $ (read endTime :: Int) - read startTime

main = do
    presses <- getLine
    let pressesNum = read presses :: Int
    if pressesNum `rem` 2 == 1 then
        (do
            repeatTask pressesNum (do getLine; return 0) (return 0)
            putStrLn "still running")
    else ( do
            ans <- repeatTask (pressesNum `div` 2) getAliveTime (return 0)
            print ans)
        
