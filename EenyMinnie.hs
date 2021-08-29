import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Control.Monad (mapM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let (r:n':xs) = init (C.split '\n' contents)
    let n = readInt n'
    let rhymes = length (C.split ' ' r)

    handle rhymes xs ([], []) 0

    where
        handle :: Int -> [C.ByteString] -> ([C.ByteString], [C.ByteString]) -> Int -> IO ()
        handle r unchosen (t1, t2) i
            | null unchosen = do print (length t1); mapM_ C.putStrLn (reverse t1); print (length t2); mapM_ C.putStrLn (reverse t2)
            | length t2 == length t1 = handle r (a++if null b then [] else tail b) ((unchosen !! i') : t1, t2) i'
            | otherwise = handle r (a++if null b then [] else tail b) (t1, (unchosen !! i') : t2) i'
            where
                i' = (i+r-1) `rem` length unchosen
                (a, b) = splitAt i' unchosen