import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_, when)
import Data.Char (isLetter, toLower)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    n <- getInt

    replicateM_ n handle

    where
        handle = do
            line <- C.group . C.sort . C.map toLower . C.filter isLetter <$> C.getLine
            let line' = C.concat line
            if length line == 26
                then putStrLn "pangram"
                else putStrLn ("missing " ++ filter (`C.notElem` line') ['a'..'z'])