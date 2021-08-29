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
        names <- sort' ([], []) <$> replicateM n (C.dropWhileEnd isSpace <$> C.getLine)
        
        printf "SET %d\n" x
        C.putStr names

        main' (x+1)

        where
            sort' :: ([C.ByteString], [C.ByteString]) -> [C.ByteString] -> C.ByteString
            sort' (a, b) [] = C.append (C.unlines . reverse $ a) (C.unlines b)
            sort' (a, b) x@[v] = C.append (C.append (C.unlines . reverse $ a) (C.unlines x)) (C.unlines b)
            sort' (a, b) (x:x':xs) = sort' (x:a, x':b) xs

main = main' 1
        

        
