import Control.Monad(replicateM)
import Data.List (elemIndex)

toInt x = read x :: Int

main = do
    n <- toInt <$> getLine
    replicateM n (do
            l <- getLine
            case l of
                ('S':'i':'m':'o':'n':' ':'s':'a':'y':'s':' ':a) -> putStrLn a
                _ -> return ()
        )