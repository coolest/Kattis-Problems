import Control.Monad (replicateM)

f = do
    x:xs <- map (\x -> read x :: Int) . words <$> getLine 
    print (sum xs - length xs + 1)
    
main = getLine >>= \x -> replicateM (read x :: Int) f