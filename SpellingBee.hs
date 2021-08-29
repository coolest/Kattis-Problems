import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import Data.Foldable (foldrM)

isASpecifiedLetter m s (a, b) x
    | m == x = (True, b)
    | S.member x s = (a, b)
    | otherwise = (a, False)

main = do
    contents <- C.getContents

    let (x:xs) = C.lines contents
    let (l:ls) = C.unpack x
    
    let solve = isASpecifiedLetter l (S.fromList ls)
    let ans = foldl (\acc x -> 
                        if C.length x >= 4 && C.foldl solve (False, True) x == (True, True)
                            then x:acc
                            else acc
                    ) [] xs

    foldrM (\a b -> C.putStrLn a) () ans