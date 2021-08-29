import Control.Monad (replicateM)

f p = getLine 
    >>= (\[w, l] -> return $ p*w*l) 
        . map (\x -> read x :: Float) 
        . words

main = do
    price <- (\x -> read x :: Float) <$> getLine
    costs <- getLine >>= (`replicateM` f price) . (\x -> read x :: Int)
    print . sum $ costs