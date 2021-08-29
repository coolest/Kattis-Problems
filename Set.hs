import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Text.Printf (printf)
import Data.Foldable (foldrM)

main = do
    contents <- C.getContents

    let xs = concatMap C.words 
            $ C.lines contents

    let s = S.fromList xs

    let ans = solve (s, 0) (s, 0) (s, 0) []

    if null ans
        then putStrLn "no sets"
        else foldrM (\[a, b, c] _ -> printf "%d %d %d\n" (a+1) (b+1) (c+1)) () ans

    where
        solve :: (S.Seq C.ByteString, Int) -> (S.Seq C.ByteString, Int) -> (S.Seq C.ByteString, Int) -> [[Int]] -> [[Int]]
        solve (s1, i1) (s2, i2) (s3, i3) ans
            | S.length s1 == i1 = ans
            | S.length s2 == i2 = solve (s1, i1+1) (s2, i1) (s3, i2) ans
            | S.length s3 == i3 = solve (s1, i1) (s2, i2+1) (s3, i2) ans
            | otherwise = solve (s1, i1) (s2, i2) (s3, i3+1) (if isSet then [i1, i2, i3]:ans else ans)
            where
                a = S.index s1 i1
                b = S.index s2 i2
                c = S.index s3 i3
                isSet = i1<i2&&i2<i3&& foldl (\acc i -> acc &&
                                let (aC, bC, cC) = (C.index a i, C.index b i, C.index c i) 
                                    in ((aC==bC&&aC==cC) || (aC/=bC&&bC/=cC&&aC/=cC))
                            ) True [0..3]