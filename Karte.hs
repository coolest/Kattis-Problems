import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)
import Text.Printf (printf)

main = do
    xs <- getLine

    handle xs (13, 13, 13, 13) []

    where
        handle :: String -> (Int, Int, Int, Int) -> [String] -> IO ()
        handle [] (p, k, h, t) _ = printf "%d %d %d %d\n" p k h t
        handle (s:n1:n2:xs) (p, k, h, t) pXS
            | [s, n1, n2] `elem` pXS = putStrLn "GRESKA"
            | s == 'P' = handle xs (p-1, k, h, t) ([s, n1, n2]:pXS)
            | s == 'K' = handle xs (p, k-1, h, t) ([s, n1, n2]:pXS)
            | s == 'H' = handle xs (p, k, h-1, t) ([s, n1, n2]:pXS)
            | otherwise = handle xs (p, k, h, t-1) ([s, n1, n2]:pXS)

