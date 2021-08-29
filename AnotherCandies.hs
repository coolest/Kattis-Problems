import Control.Monad (unless)

handle :: IO String
handle = do
    children <- getLine
    let
        childrenNum = read children :: Integer
        
        putInNextLine :: IO [Integer] -> IO [Integer]
        putInNextLine iolist = do
            candy <- getLine
            list <- iolist
            return $ (read candy :: Integer):list

        loopAdd :: Integer -> IO [Integer] -> IO [Integer]
        loopAdd n list = do
            if n == 0
                then list
                else loopAdd (n-1) (putInNextLine list)

        in ( do
                candies <- loopAdd childrenNum (return [])
                if sum candies `rem` childrenNum == 0
                    then return "YES"
                    else return "NO"
        )

runCase :: Integer -> IO ()
runCase casesLeft = do
    output <- handle
    putStrLn output
    Control.Monad.unless
        (casesLeft == 1)
        (
            do
                getLine -- whitespace
                runCase (casesLeft-1)
        )

main = do
    cases <- getLine
    getLine -- whitespace

    runCase (read cases :: Integer)