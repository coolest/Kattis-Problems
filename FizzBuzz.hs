import Control.Monad (mapM_)

f [x, y, 0] ans = ans
f [x, y, n] ans = f [x, y, n-1] ((s1++s2++s3):ans)
    where 
        s1 = if n `mod` x /= 0 && n `mod` y /= 0 then show n else ""
        s2 = if n `mod` x == 0 then "Fizz" else ""
        s3 = if n `mod` y == 0 then "Buzz" else ""

main = getLine 
    >>= mapM_ putStrLn
        . (`f` [])
        . map (\x -> read x :: Int) 
        . words
