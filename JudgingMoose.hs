import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInts = map (fst . strip . C.readInt) . C.words <$> C.getLine

bigger a b 
    | a > b = a
    | otherwise = b

main = do
    [l, r] <- getInts
    if l /= r
        then putStrLn $ "Odd " ++ show (2 * bigger l r)
        else if l == 0 
            then putStrLn "Not a moose"
            else putStrLn $ "Even " ++ show (l*2)
