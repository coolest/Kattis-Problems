import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)

main = do
    list <- replicateM 5 C.getLine
    
    let blimps = snd $ foldl (\(i, a) x -> if C.isInfixOf (C.pack "FBI") x then (i+1, show i : a) else (i+1, a)) (1, []) list
    
    if null blimps
        then putStrLn "HE GOT AWAY!"
        else putStrLn . unwords . reverse $ blimps