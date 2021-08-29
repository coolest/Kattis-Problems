import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let (l1:l2:_) = init (C.split '\n' contents)

    let (a:b:c:_) = map (fromIntegral . readInt) (C.split ' ' l1)
    let (ra:rb:rc:_) = map (fromIntegral . readInt) (C.split ' ' l2)
    let t = minimum [a / ra, b / rb, c / rc] :: Float

    printf "%.6f %.6f %.6f" (ra * (a / ra - t)) (rb * (b / rb - t)) (rc * (c / rc - t))