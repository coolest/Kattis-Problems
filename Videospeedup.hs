import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = map C.words 
            $ C.lines contents

    let [n, p, k] = map (fromIntegral  . readInt) . head $ xs
    let ts = map (fromIntegral . readInt) . last $ xs

    let (ans, _, _) = foldl (\(ans, pX, c) x -> (ans+(x-pX)+(x-pX)*c*p/100.0, x, c+1)) (0.0, 0, 0) (ts++[k])

    print ans
    