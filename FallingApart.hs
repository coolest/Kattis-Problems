import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)
import Data.List (sort)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

main = do
    getLine
    ints <- sort <$> getInts

    -- (index, girl, dude)
    let (_, girl, dude) = foldr (\x (i, g, d) -> if even i then (i+1, g+x, d) else (i+1, g, d+x)) (0, 0, 0) ints :: (Int, Int, Int)

    printf "%d %d" girl dude