import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst. fromJust . C.readInt

main = do
    contents <- C.getContents

    let lines = init (tail (C.split '\n' contents))
    handle lines

    where
        handle :: [C.ByteString] -> IO ()
        handle [] = return ()
        handle (x:xs) = do
            let n = readInt x
            let info = map (map (read . C.unpack) . C.split ' ') (take n xs) :: [[Double]]
            let (_, x, y) = foldl (\(ang, x, y) [aInc, d] -> let nAng = ang+(aInc*pi/180) in (nAng, x+(cos(nAng))*d, y+(sin(nAng))*d)) (90*pi/180, 0, 0) info
            
            printf "%f %f\n" x y

            handle (drop n xs)