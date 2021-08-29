import Control.Monad (unless)
import Data.Char (digitToInt)

findP p x s = if nS == s then p else findP (p+1) x s
    where 
        n = p*x
        nS = sum $ map digitToInt (show n)

main = do
    n <- getLine
    unless (n=="0") $ do
        let s = sum $ map digitToInt n
        print $ findP 11 (read n) s
        main
