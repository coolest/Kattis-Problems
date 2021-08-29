import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)
import Data.Maybe (fromJust)

readInt = fst. fromJust . C.readInt

main = do
    contents <- C.getContents
    
    let r:a:_ = map readInt (C.split '\n' contents)

    let m = a `rem` r
    let c = (a+m) `div` r

    let stars1 = replicate c '*'
    let stars2 = replicate (c-1) '*'

    replicateM_ (if m /= 0 then m else r) (putStrLn stars1)
    replicateM_ (if m /= 0 then r-m else m) (putStrLn stars2)