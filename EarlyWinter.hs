import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (C.split '\n' contents)

    let [n, dm] = map readInt 
            $ C.words . head $ xs
    let ds = map readInt
            $ C.words . last $ xs
    let ans = foldl (\(b, acc) x -> if b then (b, acc) else if x <= dm then (True, acc) else (b, acc+1)) (False, 0) ds
            :: (Bool, Int)

    if fst ans
        then printf "It hadn't snowed this early in %d years!" (snd ans)
        else putStrLn "It had never snowed this early!"

