import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

readInt = fst. fromJust . C.readInt
strip ((S.:<) a as) = a

handle :: S.Seq Int -> S.Seq Int -> Int -> Int -> IO (S.Seq Int)
handle seq is p n = do print seq; print is; if finished then return seq else handle (S.adjust (\a -> a-1) p seq) (if v == 6 then S.update p 2 is else (if v == 1 then S.update p 0 is else is)) p' n
    where 
        p' = nextI is p n True
        v = S.index seq p
        finished = v <= 1 && length (S.findIndicesL (<=0) is) == (n-1)

nextI :: S.Seq Int -> Int -> Int -> Bool -> Int
nextI xs p inc rs = if inc <= 0 then p else nextI ((S.|>) (S.drop 1 xs) x) (p' `rem` S.length xs) (inc-x) False
    where 
        x = let x = strip (S.viewl xs) in if rs && x == 2 then 1 else x
        p' = if x == 0 then p else p+1


main = do
    contents <- C.getContents

    let [s, n] = map readInt (C.split ' ' contents)

    let xs = S.fromList (replicate n 6)
    let is = S.fromList (replicate n 1)
    --print (nextI is 0 n)

    ans' <- (handle xs is 0 n)
    let ans = snd $ foldl (\(a, ans) x -> if x > 0 then (a+1, a) else (a+1, ans)) (1, 1) (S.reverse ans')
    print ans

    

    