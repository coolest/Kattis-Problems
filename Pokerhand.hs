import qualified Data.ByteString.Char8 as C
import Data.List

getWords = C.words <$> C.getLine

main = do
    ranks <- group . sort . map C.head <$> getWords
    print $ foldl (\a x -> if length x > a then length x else a) 0 ranks