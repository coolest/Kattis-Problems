import qualified Data.ByteString.Char8 as C
import Data.List (permutations)
import Data.Maybe (fromJust)

readInt = fst. fromJust . C.readInt

main = do
    n <- getLine

    let ns = filter (\a -> a > n) (permutations n)
    
    case ns of
        [] -> putStrLn "0"
        xs -> putStrLn (minimum xs)
