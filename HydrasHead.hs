import qualified Data.ByteString.Char8 as C
import Control.Monad (unless)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

simulate h t a
    | t > 1 || (h /= 1 && t == 2 && t /= 0) = simulate (h+1) (t-2) (a+1)
    | h > 1 && h /= 0 = simulate (h-2) t (a+1)
    | t == 1 || h == 1 = simulate h (t+1) (a+1)
    | otherwise = a

main = do
    [h, t] <- getInts

    unless (h == 0 && t == 0) $ (\h t -> do
        if odd h && t == 0
            then print (-1)
            else print (simulate h t 0)

        main) h t