import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.List (group, sort, permutations)
import Control.Monad (mapM_)

main = do
    contents <- C.getContents

    let xs = concatMap C.words
            $ C.lines contents

    let ans = solve [] xs (reverse xs)

    mapM_ (C.putStrLn . head) (group . sort $ ans)

    where
        solve :: [[C.ByteString]] -> [C.ByteString] -> [C.ByteString] -> [C.ByteString]
        solve ans [] [] = concat ans
        solve ans (x:xs) (r:rs) = solve (aX:aR:ans) xs rs
            where
                aX = map (C.append x) xs
                aR = map (C.append r) rs
    
    