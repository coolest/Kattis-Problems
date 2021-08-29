import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init
            . C.transpose . tail
            $ C.lines contents

    let ans = foldl (\(s, ans) bstr -> let newSet = C.foldl (\a c -> S.insert c a) s bstr
                in 
                    if C.foldl (\a c -> a && S.notMember c s) True bstr 
                        then (newSet, ans+1) 
                        else (newSet, ans)
                    ) (S.empty, 0) xs
                    
    print . snd $ ans