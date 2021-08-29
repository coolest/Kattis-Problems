import Data.Char (ord, chr)

toRange x
    | x > 90 = 64 - (90 - x)
    | x < 65 = 91 - (65 - x)
    | otherwise = x

main = do
    e <- map ((+ (-65)) . ord) <$> getLine
    k <- map (*(-1)) . scanl1 (\a b -> if a < 0 then b else b*(-1)) . map ((+ (-65)) . ord) <$> getLine
    putStrLn $ map (chr . toRange . (+65) . uncurry (+)) (zip e k)