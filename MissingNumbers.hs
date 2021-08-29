import qualified Data.ByteString.Char8 as C
import Control.Monad (mapM_, when)
strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    a <- handle n 0 True

    when a $ do
        putStrLn "good job"

    where
        handle 0 _ a = return a
        handle c pN a = do
            nN <- getInt
            
            if nN - pN == 1
                then handle (c-1) nN a
                else do
                    mapM_ print [(pN+1) .. (nN-1)]
                    handle (c-1) nN False