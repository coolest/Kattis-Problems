import Control.Monad (replicateM)

main = do
    merges <- (\x -> read x :: Int) <$> getLine
    n <- (\x -> x-merges+1) . sum <$> replicateM merges ((\x -> read x :: Int) <$> getLine)
    print n
