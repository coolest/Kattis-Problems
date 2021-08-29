import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

ws = C.pack "WWW"
bs = C.pack "BBB"

main = do
    n <- getInt
    board <- replicateM n C.getLine
    let board' = C.transpose board

    if check board board'
        then print 1
        else print 0

    where
        check :: [C.ByteString] -> [C.ByteString] -> Bool
        check [] [] = True
        check (x:xs) (x':xs') = length ('W' `C.elemIndices` x) == length ('B' `C.elemIndices` x) 
                                    && length ('W' `C.elemIndices` x') == length ('B' `C.elemIndices` x')
                                    && not (bs `C.isInfixOf` x || bs `C.isInfixOf` x')
                                    && not (ws `C.isInfixOf` x || ws `C.isInfixOf` x') 
                                    && check xs xs'
