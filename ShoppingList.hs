import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List (sort)
import Control.Monad (mapM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle xs

    where
        handle ls = do
            let (x:xs) = map (S.fromList . C.split ' ') ls
            let items = sort $ foldl (\a x -> if foldl (\a xs -> S.member x xs && a) True xs then x:a else a) [] x
            
            print (length items)
            mapM_ C.putStrLn items