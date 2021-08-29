import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)
import Data.List (groupBy, sortBy, sort, group)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    [r, c] <- getInts
    crossword <- replicateM r C.getLine

    let ans1 = minimum . head . group . sort $ concatMap (filter (\x -> C.length x > 1) . C.split '#') crossword
    let ans2 = minimum . head . group . sort $ concatMap (filter (\x -> C.length x > 1) . C.split '#') (C.transpose crossword)
    
    if ans1 > ans2
        then C.putStrLn ans2
        else C.putStrLn ans1

