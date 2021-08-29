import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, mapM)
import Data.List

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

trd (_, _, x) = x

main = do
    [r, c] <- getInts
    h:l <- replicateM r getLine

    let list@(h':l') = scanl (\x' x -> (fst x' + 1, x)) (0, h) l
    let ans = filter (>= 0) . sort $ foldl (\a (i, x) -> a ++ trd (foldl (f list) (i, 0, []) x)) [] list
    
    mapM (\x -> print . length $ filter (==x) ans) [0..4]

    where 
        f l t@(i, i', a) x = 
            if (length l - 1) == i || (length (snd $ head l) - 1) == i'
                then (i, i'+1, (-1) : a)
                else f' l t x
                    where
                        f' l (i, i', a) x =
                            let
                                r = snd (l !! i) !! (i'+1)
                                b = snd (l !! (i+1)) !! i'
                                rb = snd (l !! (i+1)) !! (i'+1)
                                xs = [x, r, b, rb]
                            in if r /= '#' && b /= '#' && rb /= '#' && x /= '#'
                                then (i, i'+1, foldl (\a x -> if x == 'X' then a+1 else a) 0 xs : a)
                                else (i, i'+1, (-1) : a)
