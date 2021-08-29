import Control.Monad 

strip (Just a) = a

getWords = words <$> getLine
toInt x = read x :: Int

points = [
    (True, [('A', 11), ('K', 4), ('Q', 3), ('J', 20), ('T', 10), ('9', 14), ('8', 0), ('7', 0)]),
    (False, [('A', 11), ('K', 4), ('Q', 3), ('J', 2), ('T', 10), ('9', 0), ('8', 0), ('7', 0)])]

main = do
    [n_str, [c]] <- getWords

    ans <- sum <$> replicateM (4 * toInt n_str) (f c)
    print ans

    where 
        f c = do
            a <- getChar
            b <- getChar
            getChar

            return . strip . lookup a . strip $ lookup (c == b) points


