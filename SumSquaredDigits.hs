import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

main = do
    n <- getInt
    replicateM n f

    where
        f = do
            [n, b, x] <- getInts

            putStrLn (show n ++ " " ++ show (f' b x 0))

            where 
                f' b 0 a = a
                f' b x a = f' b ((x - nA) `div` b) (a+nA*nA)
                    where nA = x `rem` b
