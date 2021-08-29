import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, mapM_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt
    replicateM n f

    where
        f = do
            l <- getLine
            l' <- getLine

            putStrLn l
            putStrLn l'
            mapM_ (\(a, b) -> if a == b then putChar '.' else putChar '*') (zip l l')
            putChar '\n'