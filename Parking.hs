import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let (x:xs) = init (C.split '\n' contents)
    let times = map ((\(a:b:_) -> (a, b)) . map readInt . C.split ' ') xs
    let payments = map readInt (C.split ' ' x)

    print (solve 1 payments times 0)

    where
        solve 100 _ _ ans = ans
        solve n p@(a:b:c:_) xs ans = solve (n+1) p xs (ans+pay)
            where pay = 
                    case foldl (\a (s, e) -> if n >= s && n < e then a+1 else a) 0 xs of
                        0 -> 0
                        1 -> a
                        2 -> b*2
                        3 -> c*3


