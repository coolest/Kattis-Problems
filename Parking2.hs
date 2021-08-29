import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInt = fst . strip . C.readInt <$> C.getLine
getInts = map (fst . strip . C.readInt) . C.words <$> C.getLine

qs [] = []
qs (x:xs) = qs (filter (<=x) xs) ++ [x] ++ qs (filter (>x) xs)

main = do
    cases <- getInt
    replicateM cases f

    where 
        f = do
            n <- getInt
            ps <- qs <$> getInts
            
            let ans = (2*) . fst $ foldl (\(a, x') x -> (a+x-x', x)) (0, head ps) ps
            print ans


