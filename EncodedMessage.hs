import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM, forM_)
import Data.List

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

rInt :: Float -> Int
rInt = round

main = do
    n <- getInt
    replicateM n f

    where 
        f = do
            line <- getLine

            let n = rInt . sqrt . fromIntegral . length $ line
            let decoded = reverse
                    . concat
                    . transpose  
                    . reverse
                    . groupBy (\a a' -> fst a == fst a') 
                    . map (\x -> ((fst x-1) `div` n + 1, snd x)) 
                    $ scanr (\x x' -> (fst x' + 1, x)) (1, last line) (init line)

            putChar '\n'
            mapM_ (putChar . snd) decoded

            -- aaaaaaaaa this was inefficient but idk what i couldve done differently