import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.List (sortBy)
import Control.Monad (mapM_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt
    
    ans <- sortBy (\(_, a) (_, b) -> a `compare` b) . M.elems <$> handle n M.empty

    print $ length ans
    mapM_ (\(_, bstr) -> C.putStrLn bstr) ans

    where
        handle :: Int -> M.Map C.ByteString (Int, C.ByteString) -> IO (M.Map C.ByteString (Int, C.ByteString))
        handle 0 hm = return hm
        handle n hm = do
            [name, pts, key] <- C.split ' ' <$> C.getLine
            let intPts = readInt pts
            if M.notMember key hm
                then handle (n-1) (M.insert key (intPts, name) hm)
                else handle (n-1) (M.adjust (\v -> if fst v > intPts then v else (intPts, name)) key hm)
