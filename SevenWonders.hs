import qualified Data.ByteString.Char8 as C
import Data.Char

qs [] = []
qs (x:xs) = qs (filter (<= x) xs) ++ [x] ++ qs (filter (>x) xs)

main = do
    list <- map C.length . C.group . C.dropWhile isSpace . C.sort <$> C.getLine -- apparently no export for C.dropSpace
    
    let sets = if length list == 3 then head $ qs list else 0
    print (7*sets + sum (map (^2) list))
