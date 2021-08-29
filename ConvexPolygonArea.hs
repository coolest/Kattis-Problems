import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Control.Monad (mapM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init . tail 
            $ C.split '\n' contents

    let ans = map (solve 0 . tail . map readInt . C.words) xs

    mapM_ (print . (/2.0) . fromIntegral) ans

    where
        solve :: Int -> [Int] -> Int
        solve ans [x1, y1] = ans
        solve ans (x1:y1:x2:y2:pts) = if ans == 0 
            then solve (ans+(x1*y2-x2*y1)) ((x2:y2:pts) ++ [x1, y1]) 
            else solve (ans+(x1*y2-x2*y1)) (x2:y2:pts)