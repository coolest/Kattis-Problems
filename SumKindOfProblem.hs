import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

r :: Double -> Int
r x = round x

main = do
    n <- getInt

    replicateM_ n handle

    where
        handle = do
            [c, n] <- getInts

            let n' = fromIntegral n :: Double
            let a1 = r ((n'^2) / 2 + n'/2)
            let a2 = n^2
            let a3 = n^2 + n

            printf "%d %d %d %d\n" c a1 a2 a3
