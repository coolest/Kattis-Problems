import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

word1 = C.pack "pink"
word2 = C.pack "rose"

main = do
    n <- getInt

    ans <- handle n 0
    if ans == 0 
        then putStrLn "I must watch Star Wars with my daughter"
        else print ans

    where
        handle 0 ans = return ans
        handle n ans = do
            line <- C.map toLower <$> C.getLine
            if C.isInfixOf word1 line || C.isInfixOf word2 line
                then handle (n-1) (ans+1)
                else handle (n-1) ans
