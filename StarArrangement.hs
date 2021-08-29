import qualified Data.ByteString.Char8 as C
import Control.Monad (when)
import Data.List

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt
    putStrLn $ show n ++ ":"

    countdown n ((n+1) `div` 2, n `div` 2) []

    where
        countdown n (1, 1) a = mapM putStrLn a
        countdown n (a1, a2) a = if r == a1 || r == 0
            then countdown n a' (formatA:a)
            else countdown n a' a

            where 
                r = n `rem` (a1+a2)
                a' = if a1 == a2 then (a1, a2-1) else (a1-1, a2)
                formatA = intercalate "," $ map show [a1, a2]
