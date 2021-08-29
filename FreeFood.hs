import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)
import qualified Data.Sequence as S

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = (\[init, end] -> (init, end)) . map readInt . C.words <$> C.getLine

days = S.fromList $ replicate 365 0

main = do
    n <- getInt

    ans <- handle days . unzip <$> replicateM n getInts
    print ans

    where
        handle days ([], []) = foldl (\a n -> if n > 0 then a+1 else a) 0 days
        handle days (hi:inits, he:ends) = handle (incrementRange (he) (hi-1) days) (inits, ends)
            where 
                incrementRange end i seq
                    | end == i = seq
                    | otherwise = incrementRange end (i+1) (S.adjust (+1) i seq)

