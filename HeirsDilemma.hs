import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
import Data.List (permutations)
import Data.Char (digitToInt)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

check x = foldl (\a c -> a && xINT `mod` digitToInt c == 0) True x
    where xINT = read x

-- not mine (or i had to understand it first) :: https://www.reddit.com/r/haskell/comments/5qav1r/determining_whether_a_list_contains_duplicates/
-- looked up cuz Data.List.nub = O(n^2)
hasDuplicatesWith :: Ord a => Set.Set a -> [a] -> Bool
hasDuplicatesWith seen [] = False -- base case: empty lists never contain duplicates
hasDuplicatesWith seen (x:xs) =
    -- If we have seen the current item before, we can short-circuit; otherwise,
    -- we'll add it the the set of previously seen items and process the rest of the
    -- list against that.
    x `Set.member` seen || hasDuplicatesWith (Set.insert x seen) xs

hasDuplicates = hasDuplicatesWith Set.empty
--

main = do
    [min, max] <- getInts

    print $ handle min max min 0

    where
        handle :: Int -> Int -> Int -> Int -> Int
        handle min max n a
            | n > max = a
            | '0' `elem` strN || hasDuplicates strN = handle min max (n+1) a
            | check strN = handle min max (n+1) (a+1)
            | otherwise = handle min max (n+1) a
            where strN = show n
