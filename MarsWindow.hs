import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let x = readInt contents

    let ans = solve 2018 4 x
    
    if ans
        then putStrLn "yes"
        else putStrLn "no"

    where
        solve y r x
            | y > x = False
            | y == x = True
            | y < x = if r+2 > 12
                then solve (y+3) (r-10) x
                else solve (y+2) (r+2) x

    