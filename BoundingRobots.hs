import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)
import Control.Monad (replicateM, unless)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.split ' ' <$> C.getLine

toVec :: [C.ByteString] -> (Int, Int)
toVec [dir, amt]
    | dir == C.pack "r" = (readInt amt, 0)
    | dir == C.pack "l" = (negate (readInt amt), 0)
    | dir == C.pack "u" = (0, readInt amt)
    | dir == C.pack "d" = (0, negate (readInt amt))

clamp n min max
    | n < min = min
    | n > max = max
    | otherwise = n

main = do
    [w, l] <- getInts

    unless (w == 0 && l == 0) $ (\w l -> do 
        handle w l
        main
        ) (w-1) (l-1)

    where
        handle w l = do
            n <- getInt
            vecs <- replicateM n (toVec . C.split ' ' <$> C.getLine)

            let robotThink@(rx, ry) = foldl (\(x, y) (x', y') -> (x+x', y+y')) (0, 0) vecs :: (Int, Int)
            let actual@(ax, ay) = foldl (\(x, y) (x', y') -> (clamp (x+x') 0 w, clamp (y+y') 0 l)) (0, 0) vecs :: (Int, Int)

            printf "Robot thinks %d %d\nActually at %d %d\n" rx ry ax ay