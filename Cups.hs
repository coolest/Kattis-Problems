import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, mapM_)
import Data.List (sortBy)
import Data.Maybe (isJust)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getLine' = C.words <$> C.getLine

isInt = isJust . C.readInt

main = do
    n <- getInt
    sizes <- sortBy (\a b -> fst a `compare` fst b) 
        . map (\[a, b] -> if isInt b then (readInt b * 2, a) else (readInt a, b))
        <$> replicateM n getLine'
    mapM_ (C.putStrLn . snd) sizes
