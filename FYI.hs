import Control.Monad (replicateM)

main = do
    str <- replicateM 3 getChar
    if str == "555"
        then putStrLn "1"
        else putStrLn "0"