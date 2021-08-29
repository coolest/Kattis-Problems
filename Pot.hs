import Control.Monad (replicateM)
import Data.Char (digitToInt)

main = do
    cases <- getLine >>= \x -> return (read x :: Int)
    ans <- replicateM cases (do
        info <- getLine >>= \x -> return (read (init x) :: Int, digitToInt $ last x)
        return $ uncurry (^) info)
    
    print . sum $ ans