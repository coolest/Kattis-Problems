import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

data Event = Enter Int | Leave Int

eventRejected (Leave _) _ _ = False
eventRejected (Enter n) p max = n+p > max

doEvent (Leave n) x = x-n
doEvent (Enter n) x = n+x

toEvent [name, n]
    | name == C.pack "enter" = Enter (readInt n)
    | otherwise = Leave (readInt n)

--
strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    [max, n] <- getInts
    
    events <- replicateM n (toEvent . C.split ' ' <$> C.getLine)

    print . fst $ foldl (\(a, x) e -> if eventRejected e x max then (a+1, x) else (a, doEvent e x)) (0, 0) events
