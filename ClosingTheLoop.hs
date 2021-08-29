import qualified Data.ByteString.Char8 as C
import Data.List (sortBy, groupBy)
import Control.Monad (unless, when)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

getFstLetter str = if strip (C.elemIndex 'B' str) < strip (C.elemIndex 'R' str) then 'B' else 'R'

main = do
    n <- getInt

    handle 1 n

    where
        handle caseNum max = do
            n <- getInt
            l <- C.getLine
            
            if check l
                then printf "Case #%d: %d\n" caseNum (onCheckSuccess l)
                else printf "Case #%d: %d\n" caseNum (0 :: Int)

            unless (caseNum == max) $ do
                handle (caseNum+1) max

            where
                check l = C.elem 'R' l && C.elem 'B' l
                onCheckSuccess l = do
                    let br@[b, r] = groupBy (\a b -> C.last a == C.last b) 
                            . sortBy (\a b -> if C.last a == C.last b then readInt (C.take (C.length b - 1) b) `compare` readInt (C.take (C.length a - 1) a) else C.last a `compare` C.last b)
                            $ C.words l

                    let n = if length b < length r then length b else length r

                    foldl (\a bstr -> a+readInt (C.take (C.length bstr - 1) bstr)) 0 (take n b) + foldl (\a bstr -> a+readInt (C.take (C.length bstr - 1) bstr)) 0 (take n r) - n*2