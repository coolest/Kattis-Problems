import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Char (ord, chr)
import System.IO (isEOF)
import Control.Monad (unless)

morse = S.fromList $ map C.pack [".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..", "..--", ".-.-", "---.", "----"]
getOrd x
    | x == '_' = 26
    | x == ',' = 27
    | x == '.' = 28
    | x == '?' = 29
    | otherwise = ord x - ord 'A'

ordToChr Nothing = '+'
ordToChr (Just x)
    | x == 26 = '_'
    | x == 27 = ','
    | x ==  28 = '.'
    | x == 29 = '?'
    | otherwise = chr (x + ord 'A')

main = do
    eof <- isEOF
    unless (eof) $ do
        msg <- C.getLine

        let (morseCoded, key) = fst $ C.mapAccumL (\a c -> charToMorse a c) (C.empty, []) msg
        let (decoded, _) = foldl (\(str, code) x -> (str ++ [(morseToChar code x)], C.drop x code)) ("", morseCoded) (key)
        
        putStrLn decoded
        main
        
        where
            morseToChar str l = ordToChr $ (S.elemIndexL morseChar morse)
                where morseChar = C.take l str
            charToMorse (code, key) c = ((C.append code str, (C.length str) : key), c)
                where 
                    str = S.index morse (getOrd c)
        
