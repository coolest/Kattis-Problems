import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let ([_, p]:xs:_) = map (map readInt . C.words)
            $ C.lines contents
    
    let ans = solve p 0 xs

    print ans

    where
        solve :: Int -> Int -> [Int] -> Int
        solve 1 ans (a:b:c:d:xs) = if a==b&&b==c&&c==d
            then solve 1 (ans+2) (b:c:d:xs)
            else solve 1 (ans+1) (b:c:d:xs)
        solve 1 ans xs = ans+length xs

        solve 2 ans (a:b:xs) = if a==b
            then solve 2 (ans+1) (b:xs)
            else solve 2 ans (b:xs)
        solve 2 ans [x] = ans

        solve 3 ans (a:b:c:xs) = case (a==b&&b==(c-1), (b+1)==a) of
            (True, True) -> solve 3 (ans+2) (b:c:xs)
            (False, False) -> solve 3 ans (b:c:xs)
            (_, _) -> solve 3 (ans+1) (b:c:xs)
        solve 3 ans [a, b] = if (b+1)==a
            then ans+1
            else ans
        
        solve 4 ans (a:b:c:xs) = case ((a-1)==b&&b==c, (a+1)==b) of
            (True, True) -> solve 4 (ans+2) (b:c:xs)
            (False, False) -> solve 4 ans (b:c:xs)
            (_, _) -> solve 4 (ans+1) (b:c:xs)
        solve 4 ans [a, b] = if (a+1)==b
            then ans+1
            else ans

        solve 5 ans (a:b:c:xs) = case ((a==b&&b==c) || (a==c&&(b+1)==a), (a+1)==b||(b+1)==a) of
            (True, True) -> solve 5 (ans+2) (b:c:xs)
            (False, False) -> solve 5 ans (b:c:xs)
            (_, _) -> solve 5 (ans+1) (b:c:xs)
        solve 5 ans [a, b] = if (a+1)==b||(b+1)==a
            then ans+1
            else ans

        solve 6 ans (a:b:c:xs) = case ((c==b&&b==(a+1))||(a==b&&b==c), a==b||(a-2)==b) of
            (True, True) -> solve 6 (ans+2) (b:c:xs)
            (False, False) -> solve 6 ans (b:c:xs)
            (_, _) -> solve 6 (ans+1) (b:c:xs)
        solve 6 ans [a, b] = if a==b||(a-2)==b
            then ans+1
            else ans

        solve 7 ans (a:b:c:xs)  = case ((a==b&&b==(c+1))||(a==b&&b==c), a==b||(b-2)==a) of
            (True, True) -> solve 7 (ans+2) (b:c:xs)
            (False, False) -> solve 7 ans (b:c:xs)
            (_, _) -> solve 7 (ans+1) (b:c:xs)
        solve 7 ans [a, b] = if a==b||(b-2)==a
            then ans+1
            else ans

