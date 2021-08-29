import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Maybe (fromJust, isJust)

readInt = fst . fromJust . C.readInt

isLoop :: Int -> Int -> Char -> S.Seq C.ByteString -> Bool
isLoop i1 i2 c xs = (c == '#' && i1 > 0 && i2 > 0 && i2+1 < C.length hXs)
    && ((C.index columnAbove (i2-1) == '#' && C.index columnAbove (i2+1) == '#') 
        || (C.index column (i2-1) == '#' && C.index columnAbove (i2+1) == '#')
        || (C.index column (i2-1) == '#' && C.index columnAbove i2 == '#')
        || (C.index column (i2+1) == '#' && cond)
        || (C.index column (i2-1) == '#' && cond)
        || (C.index column (i2+1) == '#' && C.index columnAbove i2 == '#' && (isJust columnAbove2 && C.index (fromJust columnAbove2) i2 == '.')))
    where
        cond = (isJust columnBelow && C.index (fromJust columnBelow) i2 == '#') && (isJust columnBelow2 && C.index (fromJust columnBelow2) i2 == '.')
        columnBelow = if i1+1 >= S.length xs then Nothing else Just (S.index xs (i1+1))
        columnBelow2 = if i1+2 >= S.length xs then Nothing else Just (S.index xs (i1+2))
        columnAbove2 = if i1-2 < 0 then Nothing else Just (S.index xs (i1-2))
        columnAbove = S.index xs (i1-1)
        column = S.index xs i1
        (hXs S.:< _) = S.viewl xs

main = do
    contents <- C.getContents

    let xs' = S.fromList $ map (C.cons '.') (init (tail (C.split '\n' contents)))
    let (hXs S.:< _) = S.viewl xs'
    let xs = (S.<|) (C.pack $ replicate (C.length hXs) '.') xs'
    let ans = S.foldlWithIndex (\a ind1 x -> a+ snd (C.foldl (\(ind2, a') c -> (ind2+1, a'+if isLoop ind1 ind2 c xs then 1 else 0)) (0, 0) x)) 0 xs :: Int
    print ans
