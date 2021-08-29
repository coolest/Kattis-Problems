import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    replicateM n handle

    where 
        handle = do
            [n, l, d, g] <- map fromIntegral <$> getInts

            let area = n * l^2 * (1/tan(pi/n)) / 4 + n*l*d*g + (g*d)**2 * pi

            print area