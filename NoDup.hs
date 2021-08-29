import Data.Containers.ListUtils (nubOrd)

main = do
    list <- getLine >>= \x -> return $ words x
    if length (nubOrd list) == length list
        then putStrLn "yes"
        else putStrLn "no"