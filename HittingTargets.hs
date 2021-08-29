import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

-- shape
data Shape = Circle {x :: Int, y :: Int, r :: Int} 
            | Rect {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int}

circle [x1, y1, r] = Circle {x = x1, y = y1, r = r}
rect [x1, y1, x2, y2] = Rect {x1 = x1, y1 = y1, x2 = x2, y2 = y2}

construct (x:xs)
    | x == C.pack "circle" = circle xs'
    | otherwise = rect xs'
    where 
        xs' = map readInt xs

isPointInShape :: Shape -> (Int, Int) -> Bool
isPointInShape Circle {x = x, y = y, r = r} (xP, yP) = sqrt (fromIntegral ((x-xP)^2 + (y-yP)^2)) <= fromIntegral r
isPointInShape Rect {x1 = x1, y1 = y1, x2 = x2, y2 = y2} (xP, yP) = xP >= x1
                                                    && xP <= x2
                                                    && yP >= y1
                                                    && yP <= y2

-- io
strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.split ' ' <$> C.getLine

--
main = do
    n <- getInt
    shapes <- replicateM n (construct . C.split ' ' <$> C.getLine)

    m <- getInt
    shots <- replicateM m getInts

    mapM (\[x, y] -> print $ foldl (\a shape -> if isPointInShape shape (x, y) then a+1 else a) 0 shapes) shots

