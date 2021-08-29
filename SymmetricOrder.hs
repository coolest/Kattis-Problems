import qualified Data.ByteString.Char8 as C
import Control.Monad (unless, replicateM)
import Data.List (sortBy, groupBy)
import Text.Printf (printf) 
import Data.Char (isSpace, isUpper)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main' :: Int -> IO()
main' x = do
    n <- getInt
    unless (n==0) $ do
        names <- replicateM n (C.dropWhileEnd isSpace <$> C.getLine)

        let s = sortBy (\a b -> C.length a `compare` C.length b) names
        let g = groupBy (\a b -> C.length a == C.length b) s
        let p = findPattern g

        let (a, b, c) = order g p [] []
        printf "SET %d\n" x
        C.putStr . C.unlines $ a
        C.putStr . C.unlines $ b
        C.putStr . C.unlines $ c

        main' (x+1)

        where
            findPattern (h:h':g)
                | even (length h) = 1
                | odd (length h) && odd (length h') = 2
                | odd (length h) && even (length h') = 2
                | otherwise = 1
            
            order :: [[C.ByteString]] -> Int -> [C.ByteString] -> [C.ByteString] -> ([C.ByteString], [C.ByteString], [C.ByteString])
            order [] _ a a' = (a, [], reverse a')
            order [x] _ a a' = (a, x, reverse a')
            order (h:h':g) p a a'
                | p == 1 = order (h':g) (findPattern (h':g)) (a ++ take (length h `div` 2) h) (a' ++ drop (length h `div` 2) h)
                | p == 2 = order g (findPattern g) (a ++ take ((length h+1) `div` 2) h ++ take (length h' `div` 2) h') (a' ++ drop ((length h+1) `div` 2) h ++ drop (length h' `div` 2) h')
            
main = main' 1
        

        
