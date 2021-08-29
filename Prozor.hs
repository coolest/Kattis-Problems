import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Control.Monad (replicateM)
import Data.List

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

pos x = if x < 0 then x*(-1) else x
 
main = do
    [r, c, s] <- getInts
    matrix <- replicateM r (C.getLine)

    let f = S.fromList
            . dropWhileEnd (\x -> fst x == (r-1))
            . dropWhile (\x -> fst x == 0) 
            . sortBy (\a b -> if fst a == fst b then snd a `compare` snd b else fst a `compare` fst b) 
            . concat 
            $ getFlies matrix 0 c []

    let (p, amt) = findPlace f 0 (s-2) ((0, 0), S.empty)

    print amt
    C.putStrLn . C.init . C.unlines $ draw' matrix [] 0 p s
    
    where
        draw' :: [C.ByteString] -> [C.ByteString] -> Int -> (Int, Int) -> Int -> [C.ByteString]
        draw' [] ans _ _ _ = reverse ans
        draw' (m:ms) ans r p s = draw' ms ((snd $ C.mapAccumL (\a c -> (a+1, change a r c p s)) 0 m):ans) (r+1) p s
            where
                change c r ch (r', c') s
                    | (c == c' && r == r') || (c == (c'+s-1) && r == (r'+s-1)) || (c == c' && r == (r'+s-1)) || (c == (c'+s-1) && r == r') = '+'
                    | r > r' && r < (r'+s) && (c == c' || c == (c'+s-1))  = '|'
                    | c > c' && c < (c'+s) && (r == r' || r == (r'+s-1)) = '-'
                    | otherwise = ch

        getFlies [] _ _ a = a
        getFlies (h:m) r c a = getFlies m (r+1) c (map (\x -> (r, x)) is : a)
            where is = filter (\x -> x /= 0 && x /= (c-1)) (C.elemIndices '*' h)

        findPlace :: S.Seq (Int, Int) -> Int -> Int -> ((Int, Int), S.Seq (Int, Int)) -> ((Int, Int), Int)
        findPlace f i s ans@(p@(r, c), ps)
            | i == S.length f = ((r-1, c-1), S.length ps)
            | S.length correlatingPoints > S.length ps = findPlace f (i+1) s ((pR, pC), correlatingPoints)
            | otherwise = findPlace f (i+1) s ans
            where 
                (pR, pC) = S.index f i 
                correlatingPoints = foldl (\ps p@(r, c) -> if (pR+s > r && pR <= r) && (pC+s > c && pC <= c) then p S.<| ps else ps) S.empty f
