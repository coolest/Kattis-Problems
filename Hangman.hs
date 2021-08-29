import qualified Data.Set as S

main = do
    word <- S.fromList <$> getLine

    handle word 10

    where
        handle _ 0 = putStrLn "LOSE"
        handle word tries = if S.null word
                then putStrLn "WIN"
                else do
                    g <- getChar
                    if S.member g word
                        then handle (S.filter (/=g) word) tries
                        else handle word (tries-1)
                
            