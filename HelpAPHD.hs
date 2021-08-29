import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Control.Monad (replicateM)

strip (Just a) = a

readInt x = read x :: Int
getInt = fst . strip . C.readInt <$> C.getLine

getWhile f a = do
    c <- getChar
    if not . f $ c
        then return $ c:a
        else getWhile f (c:a)

main = do
    n <- getInt
    replicateM n f

    where 
        f = do
            (stopAt:int1) <- getWhile (\x -> x/='+' && x/='P') []
            (_:int2) <- getWhile (not . isSpace) []
            if stopAt == 'P' 
                then putStrLn "skipped"
                else print (readInt (reverse int1) + readInt (reverse int2))