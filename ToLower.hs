import qualified Data.ByteString.Char8 as C
import Data.Char (isUpper)
import Data.Maybe (fromJust)
import Control.Monad (mapM_)

readInt = fst. fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init $ tail (C.split '\n' contents)
    
    print $ fst (foldl (\(ans, ind) x -> if solve x ind then (ans+1, ind+1) else (ans, ind+1)) (0, 0) xs)

    where
        solve x ind = isUpper (C.head t)
            where 
                (h, t) = C.splitAt ind x
