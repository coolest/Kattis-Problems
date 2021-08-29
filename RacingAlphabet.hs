import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)
import qualified Data.Set as S

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

d :: Double
d = (60*pi)/28

ns = S.fromList (' ':'\'':['A'..'Z'])
p x 
    | x >= 0 = x
    | otherwise = x*(-1)

lenBetween v1 v2 = if a1 > a2 then a2*d else a1*d
    where 
        i1 = S.findIndex v1 ns
        i2 = S.findIndex v2 ns
        a1 = p . fromIntegral $ i1-i2
        a2 = p . fromIntegral $ 28 - p (i1-i2)

main = do
    n <- getInt

    replicateM_ n handleLine
    
    where
        handleLine = do
            line <- C.getLine
            print 
                . (+fromIntegral (C.length line)) 
                . (/15) 
                . snd 
                $ C.foldl (\(x', a) x -> (x, a + lenBetween x' x)) (C.head line, 0) (C.tail line)
