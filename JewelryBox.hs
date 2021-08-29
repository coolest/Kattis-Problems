import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.split ' ' <$> C.getLine
getInt = readInt <$> C.getLine

--
getFormulaDeriv :: Int -> Int -> [(Double, Double)]
getFormulaDeriv l w = [(12, 2), (fromIntegral ((-4)*l + (-4)*w), 1), (fromIntegral (l*w), 0)]

getVolumeFormula :: Int -> Int -> [(Double, Double)]
getVolumeFormula l w = [(fromIntegral (l*w), 1), (fromIntegral ((-2)*l), 2), (fromIntegral ((-2)*w), 2), (4, 3)]

getCriticalValues :: [(Double, Double)] -> (Double, Double)
getCriticalValues [(a, _), (b, _), (c, _)]= (
    ((-b) + sqrt (b**2 - 4*a*c)) / (2*a),
    ((-b) - sqrt (b**2 - 4*a*c)) / (2*a))


solve x [(a, pA), (b, pB), (c, pC), (d, pD)] = a*(x**pA) + b*(x**pB) + c*(x**pC) + d*(x**pD)

main = do
    n <- getInt

    replicateM_ n $ do
        [l, w] <- getInts

        let deriv = getFormulaDeriv l w
        let formula = getVolumeFormula l w
        let (sol1, sol2) = getCriticalValues deriv
        
        let ans1 = solve sol1 formula
        let ans2 = solve sol2 formula
        if ans1 > ans2
            then print ans1
            else print ans2


