import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromMaybe)

readInt = fst . fromMaybe (-1, C.empty) . C.readInt

main = do
    contents <- C.getContents

    let (n:sentence:_) = C.split '\n' contents
    let ns = C.split ' ' sentence

    if snd $ foldl (\(n, ans) bstr -> let x = readInt bstr in if x < 0 || x == n then (n+1, ans) else (n+1, False)) (1, True) ns
        then putStrLn "makes sense"
        else putStrLn "something is fishy"