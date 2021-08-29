import Data.Char (ord, chr)
import Control.Monad (unless)

toLetterIndex x = ord x - 65
wrap x = if x-26 < 65 
    then x
    else wrap (x-26)
    
main = do
    (b, e) <- (\x -> splitAt (length x `div` 2) x) . map toLetterIndex <$> getLine
    let (bRB, eRB) = (sum b, sum e)
    let (bR, eR) = (map (wrap . (+65) . (+) bRB) b, map (wrap . (+65) . (+) eRB) e)
    let ans = zipWith (\x y -> chr $ wrap (x+(y-65))) bR eR
    putStrLn ans