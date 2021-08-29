import qualified Data.ByteString.Char8 as C

isVowel x 
    | x == 'a' || x == 'e' || x == 'o' || x == 'i' || x == 'u' = True
    | otherwise = False

main = do
    line <- C.filter (\c -> c /= '}' && c /= '{'). C.scanl1 f <$> C.getLine
    C.putStrLn line

    where 
        f c c'
            | isVowel c = '}'
            | c == '}' = '{'
            | otherwise = c'
