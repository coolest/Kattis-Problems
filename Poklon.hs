import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Maybe (fromJust)
import Data.List (sortBy, groupBy, minimumBy, maximumBy)

readInt = fst . fromJust . C.readInt

solve :: [Int] -> S.Seq [[Int]] -> [(Int, Int)]
solve (a:b:_) = foldl (foldl (\acc'@((hA, hB):t) (a':b':_) -> if a' >= hA && b' <= hB then (a', b'):acc' else acc')) [(a, b)] 

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))
    let ordSeqs = S.fromList
            . groupBy (\a b -> head a == head b)
            . sortBy (\(a:b:_) (a':b':_) -> if a == a' then b' `compare` b else a `compare` a') 
            $ map (map readInt . C.split ' ') xs

    let solns = handle ordSeqs []
    print solns

    where
        handle :: S.Seq [[Int]] -> [[[(Int, Int)]]] -> [[[(Int, Int)]]]
        handle seq solns = if S.null seq then solns else handle xs (soln:solns)
            where 
                seq' = (S.<|) (tail x) xs
                (x S.:< xs) = S.viewl seq
                soln = foldl (\a n -> solve (head x) (S.drop n seq') : a) [] [0 .. S.length seq]