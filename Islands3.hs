import qualified Data.Sequence as S
import Data.Maybe (fromJust, isJust, maybe, isNothing)

getIslandCoordinatesU xs (i, j) = if c == Just 'L' || c ==  Just 'C' 
    then getIslandCoordinatesU xs (i, j+1) ++ [Just (i, j)]
    else [Nothing]
    where
        row = S.lookup i xs
        c = S.lookup j =<< row

getIslandCoordinatesD xs (i, j) = if c == Just 'L' || c ==  Just 'C' 
    then getIslandCoordinatesD xs (i, j-1) ++ [Just (i, j)]
    else [Nothing]
    where
        row = S.lookup i xs
        c = S.lookup j =<< row

getIslandCoordinates xs (i, j) = if c == Just 'L' || c ==  Just 'C' 
    then getIslandCoordinatesL xs (i-1, j) ++ [Just (i, j)] ++ getIslandCoordinatesR xs (i+1, j) 
    else [Nothing]
    where
        row = S.lookup i xs
        c = S.lookup j =<< row

        getIslandCoordinatesL xs (i, j) = if c == Just 'L' || c ==  Just 'C' 
            then getIslandCoordinatesU xs (i, j+1) ++ getIslandCoordinatesD xs (i, j-1) ++ getIslandCoordinatesL xs (i-1, j) ++ [Just (i, j)] 
            else [Nothing]
            where
                row = S.lookup i xs
                c = S.lookup j =<< row

        getIslandCoordinatesR xs (i, j) = if c == Just 'L' || c ==  Just 'C' 
            then getIslandCoordinatesU xs (i, j+1) ++ getIslandCoordinatesD xs (i, j-1) ++ getIslandCoordinatesR xs (i+1, j) ++ [Just (i, j)]
            else [Nothing]
            where
                row = S.lookup i xs
                c = S.lookup j =<< row

main = do
    contents <- getContents

    let xs = S.fromList $ map S.fromList (tail (lines contents))

    print (handle xs (0, 0) 0)
    print (getIslandCoordinates xs (0, 0))
    where
        handle :: S.Seq (S.Seq Char) -> (Int, Int) -> Int -> Int
        handle xs (i, j) ans
            | isNothing c = ans
            | c == Just 'L' = handle (foldl (\xs (Just (a, b)) -> S.adjust (S.update b 'W') a xs) xs (filter isJust (getIslandCoordinates xs (i, j)))) nextCoordinates (ans+1)
            | otherwise = handle xs nextCoordinates ans
            where
                row = S.index xs i
                c = if j == S.length row then Nothing else Just (S.index row j)
                nextCoordinates = if (i+1) == S.length xs then (0, j+1) else (i+1, j)

