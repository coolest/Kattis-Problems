import qualified Data.ByteString.Char8 as C

handle :: [C.ByteString] -> Int -> Int
handle [] n = n
handle (x:xs) n = if (C.pack "ae") `C.isInfixOf` x
    then handle xs (n+1)
    else handle xs n

main = do
    msg <- C.split ' ' <$> C.getLine

    let amt = handle msg 0
    let msgLength = fromIntegral (length msg)

    if fromIntegral amt >= (msgLength * 0.40)
        then putStrLn "dae ae ju traeligt va"
        else putStrLn "haer talar vi rikssvenska"