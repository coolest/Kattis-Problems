import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInts = map (fst . strip . C.readInt) . C.words <$> C.getLine

treasure x 
    | x >= 6 = "Gold"
    | x >= 3 = "Silver"
    | otherwise = "Copper"

victory x 
    | x >= 8 = "Province or "
    | x >= 5 = "Duchy or "
    | x >= 2 = "Estate or "
    | otherwise = ""

main = do
    [g, s, c] <- getInts
    let x = g*3+s*2+c

    putStrLn $ victory x ++ treasure x
