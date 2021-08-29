import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.List (sort, group)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

getNumRange :: C.ByteString -> [Int]
getNumRange bstr = [readInt (C.takeWhile (/='-') bstr) .. readInt (C.tail (C.dropWhile (/='-') bstr))]

getAmt :: [C.ByteString] -> (Int, [Int])
getAmt = foldl (\(a, xs) bstr -> if C.elem '-' bstr then let numRange = getNumRange bstr in (a + length numRange, numRange ++ xs) else (a+1, (readInt bstr) : xs)) (0, [])

getDups :: Int -> Int -> [Int] -> M.Map String [Int] -> Int
getDups h m s col = foldl (\a x -> if S.member x s' then a+1 else a) 0 (map head . group . sort $ (dupH' ++ dupM' ++ dupA'))
    where 
        dupH' = if dupH == Just [0] then [1..60] else if isJust dupH then fromJust dupH else []
        dupM' = if dupM == Just [0] then [1..60] else if isJust dupM then fromJust dupM else []
        dupA' = if dupA == Just [0] then [1..60] else if isJust dupA then fromJust dupA else []
        dupH = M.lookup ("0:" ++ show m) col
        dupM = M.lookup (show h ++ ":0") col
        dupA = M.lookup ("0:0") col
        s' = S.fromList (if s == [0] then [1..60] else s)

getSpsDups :: [(Int, Int, [Int])] -> Int -> Int
getSpsDups [] a = a
getSpsDups ((n1, n2, vs):xs) a = getSpsDups xs (a+foldl (\a (n1', n2', vs') -> if n1' == 0 && n2 == 0 || n1 == 0 && n2' == 0 then a+handleDups vs vs' else a) 0 xs)
    where
        handleDups as bs
            | as == [0] && bs == [0] = 60
            | as == [0] = length bs
            | bs == [0] = length as
            | otherwise = foldl (\a x -> if S.member x bs' then a+1 else a) 0 as
            where
                bs' = S.fromList bs

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))
    let ((ans1, secMap), dups') = handle xs (0, M.empty) 0
    let (dups, sps) = M.foldlWithKey (\(a, sps) k v -> let (n1, n2) = (read (takeWhile (/=':') k), read $ tail (dropWhile (/=':') k)) in (a+(if n1 /= 0 && n2 /= 0 then getDups n1 n2 v secMap else 0), if n1 == 0 || n2 == 0 then (n1, n2, v):sps else sps)) (0, []) secMap
    let spsDups = getSpsDups sps 0
    let ans2 = ans1 - dups - spsDups - dups'

    printf "%d %d\n" ans2 ans1

    where
        handle :: [C.ByteString] -> (Int, M.Map String [Int]) -> Int -> ((Int, M.Map String [Int]), Int)
        handle [] ans d = (ans, d)
        handle (l:ls) (total, secMap) d = do
            let (h:m:s:_) = map (C.split ',') (C.split ' ' (C.init l))

            let (h', hs) = if head h == C.pack "*" then (24, [0]) else getAmt h
            let (m', ms) = if head m == C.pack "*" then (60, [0]) else getAmt m
            let (s', ss) = if head s == C.pack "*" then (60, [0]) else getAmt s

            let (newMap, dup) = foldl (\a h -> let sh = show h in (foldl (\(a', d) m -> if M.member (sh ++ ':' : show m) a' then (a', d+if sh++':':show m == "0:0" then 60*24*(if ss == [0] then 60 else length ss) else (if ss == [0] then 60 else length ss)) else (M.insert (sh ++ ':' : show m) ss a', d)) a ms)) (secMap, 0) hs
            handle ls (total+h'*m'*s', newMap) (d+dup)