import Data.List (intersperse)
import Text.Printf (printf)

main = do
    let list = filter (\c -> c/=' ') <$> getLine
    handle list

    where
        handle :: IO String -> IO ()
        handle ioxs = do
            xs <- ioxs 
            if xs == "12345" then return () else handle (check1 xs >>= check2 >>= check3 >>= check4)
            where 
                check1 :: [Char] -> IO [Char]
                check1 l@[a, b, c, d, e] = if a > b then (do 
                        let xs = [b, a, c, d, e]
                        printf "%c %c %c %c %c\n" b a c d e
                        return xs)
                    else return l
                check2 :: [Char] -> IO [Char]
                check2 l@[a, b, c, d, e] = if b > c then (do 
                        let xs = [a, c, b, d, e]
                        printf "%c %c %c %c %c\n" a c b d e
                        return xs)
                    else return l
                check3 :: [Char] -> IO [Char]
                check3 l@[a, b, c, d, e] = if c > d then (do 
                        let xs = [a, b, d, c, e]
                        printf "%c %c %c %c %c\n" a b d c e
                        return xs)
                    else return l
                check4 :: [Char] -> IO [Char]
                check4 l@[a, b, c, d, e] = if d > e then (do 
                        let xs = [a, b, c, e, d]
                        printf "%c %c %c %c %c\n" a b c e d
                        return xs)
                    else return l