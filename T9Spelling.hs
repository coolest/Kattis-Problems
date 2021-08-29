import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Char (ord)
import Text.Printf (printf)
import Control.Monad (unless)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

gen x
    | x == "7" || x == "9" = [[x], x:[x], x:x:[x], x:x:x:[x]]
    | otherwise = [[x], x:[x], x:x:[x]]

ord' x 
    | x == ' ' = 0
    | otherwise = ord x - 96

translations = S.fromList $ zip (' ':['a'..'z']) (["0"]:concatMap (gen . show) [2..9])

main = do
    n <- getInt

    handle 1 n 

    where
        handle n m = do
            line <- getLine
            let tLine = scanl1 (\a b -> if last (last a) == head (head b) then " ":b else b) 
                        $ map (snd . S.index translations . ord') line

            printf "Case #%s: %s\n" (show n) (concat . concat $ tLine)
            unless (n==m) $ do
                handle (n+1) m