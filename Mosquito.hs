import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Control.Monad (mapM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = map (map readInt . C.split ' ') (init (C.split '\n' contents))
    mapM_ handle xs

    where
        solve :: [Int] -> Int
        solve (m:p:l:e:r:s:n:_) = if n == 1 then p`div`s else solve [p`div`s, l`div`r, m*e, e, r, s, (n-1)]

        handle :: [Int] -> IO ()
        handle = print . solve


