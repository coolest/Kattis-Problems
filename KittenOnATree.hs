import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.split ' ' <$> C.getLine

collectTree tree = do
    branch <- getInts
    if head branch == (-1)
        then return tree
        else collectTree (branch:tree)

main = do
    n <- getInt
    tree <- collectTree []

    putStrLn (findPath tree [n])
    
    where
        findPath :: [[Int]] -> [Int] -> String
        findPath tree (n:ns)
            | branch /= 0 = findPath tree (branch:n:ns)
            | otherwise = unwords (reverse (map show (n:ns)))
            where 
                branch = foldl (\a (x:xs) -> if n `elem` xs then x else a) 0 tree
