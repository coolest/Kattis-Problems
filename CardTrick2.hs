import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Char (intToDigit, digitToInt)
import Data.Maybe (fromJust)
import Data.List (permutations)
import Control.Monad (mapM_, unless)

readInt = fst. fromJust . C.readInt

checkAll :: [S.Seq Int] -> S.Seq Int
checkAll (x:xs) = if check x 1 then x else checkAll xs
    where
        check :: S.Seq Int -> Int -> Bool
        check xs n = S.length xs == 1 || (x == n && check (b S.>< a) (n+1))
            where 
                (a, x S.:<| b) = S.splitAt (n `mod` S.length xs) xs
            
main = do
    contents <- C.getContents

    let cases = tail (C.words contents)
    handle cases

    where
        handle :: [C.ByteString] -> IO ()
        handle [] = return ()
        handle (c:cs) = do
            let x = readInt c
            let xs = [2..x]
            let possibilities = map (\(x:xs) -> S.fromList (x:1:xs)) (permutations xs)

            let (ans S.:|> x) = checkAll possibilities
            mapM_ (\x -> putStr (show x ++ " ")) ans
            putStr $ show x

            unless (null cs) $ do
                putChar '\n'

                handle cs