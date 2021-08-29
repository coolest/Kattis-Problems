import qualified Data.ByteString.Char8 as C
import Control.Monad (mapM_, when)
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst. fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = C.split '\n' contents
    mapM_ handle (init (tail xs))

    where
        solve :: Int -> [Bool] -> [Bool]
        solve n ans = if n == 1 then ans else solve (n `div` 2) (odd n : ans)
            
        handle x = printf "%d %d/%d\n" c a b
            where
                (c:n:_) = map readInt (C.split ' ' x)
                (a, b) = foldl (\(a, b) bool -> if bool then (a+b, b) else (a, a+b)) (1, 1) (solve n []) :: (Int, Int)