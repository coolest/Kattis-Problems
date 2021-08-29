import qualified Data.Sequence as S
import Data.Char (ord, chr)

toShiftDigit :: Char -> Int
toShiftDigit x = ord x - ord 'A'

toLetter :: Int -> Char
toLetter x = chr (ord 'A' + x')
    where x' = x `mod` 26

main = do
    encrypt <- getLine
    key <- S.fromList . map toShiftDigit <$> getLine

    mapM (putChar . toLetter) (unencrypt key encrypt 0 [])

    where
        unencrypt :: S.Seq Int -> String -> Int -> [Int] -> [Int]
        unencrypt _ [] _ ans = reverse ans
        unencrypt k (x:xs) i ans = unencrypt k' xs (i+1) (letter : ans)
            where 
                letter = toShiftDigit x - S.index k i
                k' = (S.|>) k letter
