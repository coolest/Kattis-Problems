import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.List (groupBy, sortBy)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

adrian = S.fromList ['A'..'C']
bruno = S.fromList ['B', 'A', 'B', 'C']
goran = S.fromList ['C', 'C', 'A', 'A', 'B', 'B']

main = do
    n <- getInt
    (x:xs) <- head . groupBy (\a b -> fst a == fst b) . sortBy (\a b -> fst b `compare` fst a) <$> handle 0 n (0, 0, 0)
    
    let r = fst x
    let names = unlines $ map snd (x:xs)

    print r
    putStr names

    where
        handle n m (a, b, g) = do
            c <- getChar

            let aG = if S.index adrian (n `mod` S.length adrian) == c then 1 else 0
            let bG = if S.index bruno (n `mod` S.length bruno) == c then 1 else 0
            let gG = if S.index goran (n `mod` S.length goran) == c then 1 else 0

            if n /= m 
                then handle (n+1) m (a+aG, b+bG, g+gG)
                else return [(a+aG, "Adrian"), (b+bG, "Bruno"), (g+gG, "Goran")]

            
