import qualified Data.ByteString.Char8 as C
import Data.List (sort, group)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

pos x = if x < 0 then x*(-1) else x

main = do
    [n, _] <- getInts

    ps <- getInts

    let ws = tail (map head . group . sort $ concatMap (\p -> map (\x -> pos $ p-x) ps) (0:n:ps))
    mapM (printf "%d ") (ws++[n])