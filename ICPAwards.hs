import qualified Data.ByteString.Char8 as C
import Control.Monad 

strip (Just a) = a

getInt = fst . strip . C.readInt <$> C.getLine

t [a, b] = (a, b)

main = do
    n <- getInt
    winners <- replicateM n (t . C.words <$> C.getLine)
    f 12 winners
    where 
        f 0 _ = return ()
        f n ((a, b):winners) = do
            putStrLn . filter (/='"') . show $ C.unwords [a, b]
            f (n-1) (filter ((/=a) . fst) winners)
