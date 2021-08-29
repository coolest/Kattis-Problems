import Data.Containers.ListUtils (nubOrd)

main = do
    list <- getLine
    if length list == length (nubOrd list)
        then print 1
        else print 0