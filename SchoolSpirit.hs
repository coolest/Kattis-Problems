import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

calculateScore :: [Int] -> Double -> Double -> Double
calculateScore [] s i = s
calculateScore (x:xs) s i = calculateScore xs (s + 0.2*(fromIntegral x*(0.80**i))) (i+1)

calculateG list ans i = if i == length list 
    then ans
    else calculateG list (ans + calculateScore (a ++ tail b) 0.0 0) (i+1)
    where (a, b) = splitAt i list

main = do
    n <- getInt
    scores <- replicateM n getInt

    print $ calculateScore scores 0.0 0 
    print (calculateG scores 0 0 / fromIntegral (length scores))


