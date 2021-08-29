import qualified Data.Sequence as S
import Data.Char (ord, chr)

toShiftDigit :: Char -> Int
toShiftDigit x = ord x - ord 'a'

toLetter :: Int -> Char
toLetter x = chr (ord 'a' + x)

main = do
    getLine

    xs <- map toShiftDigit <$> getLine
    encrypt <- getLine

    let key = S.fromList xs
    let ans = unencrypt key encrypt 0 xs
    mapM (putChar . toLetter) (unencrypt key encrypt 0 xs)

    where
        unencrypt :: S.Seq Int -> String -> Int -> [Int] -> [Int]
        unencrypt _ [] _ ans = ans
        unencrypt k (x:xs) i ans = unencrypt k' xs (i+1) (letter : ans)
            where 
                letter = (26 + toShiftDigit x - S.index k i) `mod` 26
                k' = (S.|>) k letter
