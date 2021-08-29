import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInts = map (fst . strip . C.readInt) . C.words <$> C.getLine

main = do
    [n, c1, c2] <- getInts

    let s1 = if n-c1 > n-(n-c1) then n - c1 else n-(n-c1)
    let s2 = if n-c2 > n-(n-c2) then n - c2 else n-(n-c2)

    print $ s1*s2*4