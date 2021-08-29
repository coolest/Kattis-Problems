import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

p x = if x < 0 then x * (-1) else x

main = do
    dataPoints@[x, y, x1, y1, x2, y2] <- map fromIntegral <$> getInts

    let calcDistanceFromP = calcDistance x y
    let closestVertex = minimum [calcDistanceFromP x1 y1, calcDistanceFromP x1 y2, calcDistanceFromP x2 y1, calcDistanceFromP x2 y2]
    let closestEdgePoint = calcClosestEdgePoint dataPoints 
    
    if closestEdgePoint < closestVertex
        then print closestEdgePoint
        else print closestVertex

    where
        calcDistance x1 y1 x2 y2 = sqrt $ (x1-x2)^2 + (y1-y2)^2
        calcClosestEdgePoint [x, y, x1, y1, x2, y2]
            | x > x1 && x < x2 && y > y1 && y < y2 = minimum [p $ y-y1, p $ y-y2, p $ x-x1, p $ x-x2]
            | x > x1 && x < x2 = minimum [p $ y-y1, p $ y-y2]
            | y > y1 && y < y2 = minimum [p $ x-x1, p $ x-x2]
            | otherwise = 1000000