import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.List (sort)
import Control.Monad (zipWithM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init 
            $ C.lines contents

    handle xs M.empty

    where
        handle :: [C.ByteString] -> M.Map C.ByteString [C.ByteString] -> IO ()
        handle [] _ = return ()
        handle (x:xs) m = do 
            let elems = map sort
                    $ M.elems ans
            let keys = M.keys ans

            zipWithM_ (\a b -> do putChar '\n'; C.putStr a; putChar ' '; C.putStr . C.unwords $ b) keys elems
            putChar '\n'
            
            handle (drop n xs) M.empty

            where
                n = readInt x

                orders = map C.words 
                    $ take n xs

                ans = foldl (\acc (a:xs) -> solve acc xs a) m orders

                solve :: M.Map C.ByteString [C.ByteString] -> [C.ByteString] -> C.ByteString -> M.Map C.ByteString [C.ByteString]
                solve acc xs a = foldl (\acc x -> M.insertWith (++) x [a] acc) acc xs
    