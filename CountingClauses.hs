import Data.Char (isSpace)

getUntilSpace str = do
    c <- getChar
    if isSpace c
        then return $ reverse str
        else getUntilSpace (c:str)

main = do
    n <- read <$> getUntilSpace ""
    if n < 8
        then putStrLn "unsatisfactory"
        else putStrLn "satisfactory"
